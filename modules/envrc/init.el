;;; init.el --- Envrc module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Direnv integration for per-directory environment variables.
;; Automatically loads .envrc files via direnv, ensuring shell environment
;; variables are available in Emacs buffers.

;;; Code:

(require '+autoloads)

(require '+corelib)

(use-package envrc
  :hook (+first-file-hook . envrc-global-mode)
  :custom
  (envrc-show-summary-in-minibuffer nil) ; very noisy.

  ;; Execute org-babel source blocks in the host buffer's environment.

  :functions envrc-propagate-environment
  :config
  (use-package ob
    :functions org-babel-execute-src-block
    :config
    (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment))

  ;; Set up direnv *before* the major-mode's body executes. This ensures any
  ;; hooks or advice in the mode setup sequence predictably have the direnv
  ;; environment.

  ;; See: https://github.com/doomemacs/doomemacs/blob/21682009b155c0b67ec47100e09cad3b298aa52f/modules/tools/direnv/config.el

  :functions (envrc-global-mode-enable-in-buffer)
  :defines envrc-global-mode

  :preface
  (defun +direnv-mode-setup-h ()
    (if (not envrc-global-mode)
        (remove-hook 'change-major-mode-after-body-hook #'envrc-global-mode-enable-in-buffer)
      (remove-hook 'after-change-major-mode-hook #'envrc-global-mode-enable-in-buffer)
      (add-hook 'change-major-mode-after-body-hook #'envrc-global-mode-enable-in-buffer 100)))

  :config
  (add-hook 'envrc-global-mode-hook #'+direnv-mode-setup-h))


