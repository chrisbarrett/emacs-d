;;; init-elixir.el --- Elixir language config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-extra-root-markers "mix.exs"))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls")))

  (pushnew! find-sibling-rules
            ;; Impl -> tests
            (list (rx (group-n 1 (+? nonl)) "/lib/" (group-n 2 (+? any)) ".ex" eos)
                  (rx (backref 1) "/test/" (backref 2) "_test.exs"))

            ;; Tests -> impl
            (list (rx (group-n 1 (+? nonl)) "/test/" (group-n 2 (+? any)) "_test.exs" eos)
                  (rx (backref 1) "/lib/" (backref 2) ".ex"))))

(use-package inf-elixir :ensure t)


(provide 'init-elixir)

;;; init-elixir.el ends here
