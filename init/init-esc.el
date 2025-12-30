;;; init-esc.el --- Escape key customisation from Doom -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(autoload 'general-define-key "general")

(defvar +escape-hook nil
  "Hook functions run until success when ESC is pressed.")

(defun +escape (&optional interactive)
  "Quit things, abort things, and finish things.
Runs `+escape-hook'.

INTERACTIVE is set when called interactively."
  (interactive (list 'interactive))
  (let ((inhibit-quit t)
        (in-minibuffer? (minibuffer-window-active-p (minibuffer-window))))
    (cond
     (in-minibuffer?
      (when interactive (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))

     ;; Run all escape hooks. If any returns non-nil, then stop there.
     ((run-hook-with-args-until-success '+escape-hook))

     ;; Don't abort keyboard macros.
     ((or defining-kbd-macro executing-kbd-macro))

     ;; Fall back to keyboard-quit.
     (t
      (unwind-protect (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'+escape)
(global-set-key [remap abort-recursive-edit] #'+escape)
(general-define-key :states 'normal [escape] #'+escape)
(general-define-key :keymaps +default-minibuffer-maps [escape] #'+escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command '+escape))


(provide 'init-esc)

;;; init-esc.el ends here
