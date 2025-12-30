;;; init-erlang.el --- Erlang -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package erlang :ensure t :disabled t ; big boy that takes ages to clone
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)))

(use-package dired
  :config
  (pushnew! completion-ignored-extensions ".jam" ".vee" ".beam"))

(use-package dired-x
  :config
  (pushnew! dired-omit-extensions ".jam" ".vee" ".beam"))

(provide 'init-erlang)

;;; init-erlang.el ends here
