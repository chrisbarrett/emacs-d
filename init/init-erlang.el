;;; init-erlang.el --- Erlang -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package erlang :ensure t :disabled t ; big boy that takes ages to clone
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :init
  (with-eval-after-load 'dired
    (pushnew! completion-ignored-extensions ".jam" ".vee" ".beam"))
  (with-eval-after-load 'dired-x
    (pushnew! dired-omit-extensions ".jam" ".vee" ".beam")))

(provide 'init-erlang)

;;; init-erlang.el ends here
