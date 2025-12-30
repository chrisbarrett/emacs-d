;;; init-elixir.el --- Elixir language config  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :config
  (pushnew! find-sibling-rules
            ;; Impl -> tests
            (list (rx (group-n 1 (+? nonl)) "/lib/" (group-n 2 (+? any)) ".ex" eos)
                  (rx (backref 1) "/test/" (backref 2) "_test.exs"))

            ;; Tests -> impl
            (list (rx (group-n 1 (+? nonl)) "/test/" (group-n 2 (+? any)) "_test.exs" eos)
                  (rx (backref 1) "/lib/" (backref 2) ".ex"))))

(use-package inf-elixir :ensure t)

(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "mix.exs"))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls")))

(use-package +file-templates
  :config
  (+define-file-template-dispatcher 'elixir-ts-mode
    ((string-match-p "/lib/" (buffer-file-name))
     "elixir/lib.eld")
    ((string-match-p (rx "/test/" (+? nonl) ".exs" eos) (buffer-file-name))
     "elixir/test.eld")))

(provide 'init-elixir)

;;; init-elixir.el ends here
