;;; +hooks.el --- Core hooks -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;;; Extra UI & session lifecycle hooks

;; These hooks are cribbed from Doom--they're a great pattern for deferred
;; loading.

;; The first set are *transient* hooks; they are run only once in the Emacs
;; session, the first time a particular action is performed by the user.

(defvar +first-input-hook nil
  "Transient hook before first user input.")

(defvar +first-file-hook nil
  "Transient hook before first interactively opened file.")

(defvar +first-buffer-hook nil
  "Transient hook before first interactively opened buffer.")

;; These are set up to run just once by other hooks.

(+run-hook-once '+first-buffer-hook '(+switch-buffer-hook find-file-hook))
(+run-hook-once '+first-file-hook '(find-file-hook dired-initial-position-hook))
(+run-hook-once '+first-input-hook '(pre-command-hook))

;; The remaining hooks are executed routinely throughout the session.

(defvar +switch-buffer-hook nil
  "Hooks run after changing the current buffer.")

(defvar +switch-frame-hook nil
  "Hooks run after changing focused frame.")

(defvar +switch-window-hook nil
  "Hooks run after changing focused window.")

(defun +run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks '+switch-buffer-hook)))

(defun +run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks '+switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks '+switch-window-hook))))

(add-transient-hook! 'after-init-hook
  (add-hook 'window-selection-change-functions #'+run-switch-window-or-frame-hooks-h)
  (add-hook 'window-buffer-change-functions #'+run-switch-buffer-hooks-h)
  (add-hook 'server-visit-hook #'+run-switch-buffer-hooks-h))


;;; Local Vars Hooks - `${major-mode}-local-vars-hook'

;; Dynamically define hooks that run after a buffer's major-mode has completed
;; setup and all buffer-local variables have been resolved.
;;
;; This is a more reliable customisation point for hooks that depend on
;; buffer-local variables, including variables set via dir-locals.
;;
;; GUIDELINES: Use `${major-mode}-local-vars-hook' for any user hooks that:
;;
;; - start processes
;; - perform filesystem access
;; - access the process environment, especially PATH or `exec-path'
;; - are otherwise slow or expensive
;;
;; CRITICAL: Any hooks that attempt to launch programs or read `exec-path',
;; directly or indirectly, MUST use these local-vars-hooks. This is because
;; `exec-path' may be set as a buffer-local variable via `envrc-mode'.
;;
;; Setting `+inhibit-local-var-hooks' dynamically will inhibit these hooks. The
;; typical use-case would be to avoid expensive system access for
;; temporary/non-file buffers.

(defvar +inhibit-local-var-hooks nil
  "If non-nil, `+run-local-var-hooks-h' is suppressed.
Set this to prevent recursive hook triggering.")

(defmacro +with-inhibit-local-var-hooks (&rest body)
  "Run BODY forms with ${major-mode}-local-var-hooks inhibited."
  (declare (indent 0))
  `(let ((+inhibit-local-var-hooks t))
     ,@body))

(defun +inhibit-local-var-hooks-a (fn &rest args)
  "Apply FN to ARGS with ${major-mode}-local-var-hooks inhibited."
  (+with-inhibit-local-var-hooks
    (apply fn args)))

(defun +run-local-var-hooks-h ()
  "Run MODE-local-vars-hook after local variables are set.
This hook runs after file and directory local variables have been
applied, allowing mode-specific configuration that depends on those
settings."
  (unless (or +inhibit-local-var-hooks
              delay-mode-hooks
              (string-prefix-p " " (buffer-name)) ; hidden buffer
              )
    (+with-inhibit-local-var-hooks
      (apply #'run-hooks (nreverse
                          (seq-keep (lambda (mode)
                                      (intern-soft (format "%s-local-vars-hook" mode)))
                                    (derived-mode-all-parents major-mode)))))))

;; Run ${major-mode}-local-vars-hook after setting/changing a buffer's major
;; mode, in the 'hack local variables' buffer setup phase.

(add-hook 'hack-local-variables-hook #'+run-local-var-hooks-h)

(provide '+hooks)

;;; +hooks.el ends here
