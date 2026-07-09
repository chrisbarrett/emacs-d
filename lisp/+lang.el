;;; +lang.el --- Declarative language-wiring helper -*- lexical-binding: t; -*-

;;; Commentary:

;; `+lang-declare' is the one place the two wiring idioms a language
;; module would otherwise hand-roll — eglot activation on the mode's
;; local-vars hook and apheleia formatter registration — live, so their
;; non-obvious constraints are encoded once instead of re-derived in every
;; `modules/lang-*/init.el'.

;;; Code:

(require 'cl-lib)

(defvar apheleia-formatters)
(defvar apheleia-mode-alist)
(declare-function eglot-ensure "eglot")

(cl-defun +lang-declare (mode &key lsp formatter)
  "Declare editor wiring for major MODE.

Wire the two idioms a language module would otherwise hand-roll,
encoding each idiom's non-obvious constraint once.

When LSP is non-nil, arrange for `eglot-ensure' to run from MODE's
`<mode>-local-vars-hook' rather than its `<mode>-hook'.  The
local-vars hook fires after directory-local variables are applied,
so eglot starts with the project's buffer-local `exec-path' and
kin in effect (see `+run-local-var-hooks-h').  The hook entry is
added immediately and does not require eglot to be loaded.  MODE is
required when LSP is non-nil.

FORMATTER registers an apheleia formatter.  It is either a symbol
naming a formatter apheleia already defines, in which case only the
MODE association is added, or a cons (NAME . COMMAND) that both
registers COMMAND in `apheleia-formatters' under NAME and associates
MODE with NAME in `apheleia-mode-alist'.  When MODE is nil the
COMMAND is registered without a mode association, for modes that
select their formatter dynamically.  Registration is deferred until
apheleia loads and never forces the load."
  (when lsp
    (unless mode
      (error "+lang-declare: :lsp requires a MODE"))
    (add-hook (intern (format "%s-local-vars-hook" mode)) #'eglot-ensure))
  (when formatter
    (with-eval-after-load 'apheleia
      (cond
       ((consp formatter)
        (setf (alist-get (car formatter) apheleia-formatters) (cdr formatter))
        (when mode
          (setf (alist-get mode apheleia-mode-alist) (car formatter))))
       ((symbolp formatter)
        (unless mode
          (error "+lang-declare: a symbol :formatter requires a MODE"))
        (setf (alist-get mode apheleia-mode-alist) formatter))
       (t
        (error "+lang-declare: invalid :formatter %S" formatter))))))

(provide '+lang)

;;; +lang.el ends here
