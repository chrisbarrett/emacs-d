;;; init-system.el --- Host system integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Adds direnv support.
(use-package envrc :ensure t
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


;; Adds support for mise - https://mise.jdx.dev.
(use-package misery
  :ensure-unless-local ("~/src/chrisbarrett/emacs-misery"
                        (nursery :host github
                                 :repo "chrisbarrett/emacs-misery"))
  :hook (+first-file-hook . misery-global-mode)
  :custom
  (misery-show-summary-in-minibuffer nil))


(use-package tramp
  :config
  (pushnew! tramp-remote-path 'tramp-own-remote-path))


;; Run a hook at midnight; cleans up old buffers by default. Useful for
;; preventing an Emacs server instance from drowning in open buffers
;; throughout the work week.
;;
;; The behaviour of the buffer cleanup can be customised via the
;; `clean-buffer-list-*' variables.
(use-package midnight
  :after-call +first-file-hook +first-buffer-hook
  :config
  (midnight-mode +1))


(provide 'init-system)

;;; init-system.el ends here
