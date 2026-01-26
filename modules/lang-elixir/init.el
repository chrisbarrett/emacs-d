;;; lang-elixir/init.el --- Elixir language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Elixir development with Tree-sitter mode and LSP integration.
;; - LSP via eglot with elixir-ls
;; - Interactive REPL via inf-elixir
;; - Sibling file navigation for lib↔test
;; - Project detection via mix.exs
;; - File templates for modules and tests

;;; Code:

(require '+corelib)

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :hook (elixir-ts-mode-local-vars-hook . eglot-ensure)
  :config
  (pushnew! find-sibling-rules
            ;; Impl -> tests
            (list (rx (group-n 1 (+? nonl)) "/lib/" (group-n 2 (+? any)) ".ex" eos)
                  (rx (backref 1) "/test/" (backref 2) "_test.exs"))
            ;; Tests -> impl
            (list (rx (group-n 1 (+? nonl)) "/test/" (group-n 2 (+? any)) "_test.exs" eos)
                  (rx (backref 1) "/lib/" (backref 2) ".ex"))))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls")))

(use-package inf-elixir)

(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "mix.exs"))

;; File templates
(use-package +file-templates
  :config
  (+define-file-template-dispatcher 'elixir-ts-mode
    ((string-match-p "/lib/" (buffer-file-name))
     "elixir/lib.eld")
    ((string-match-p (rx "/test/" (+? nonl) ".exs" eos) (buffer-file-name))
     "elixir/test.eld")))

(provide 'lang-elixir-init)

;;; lang-elixir/init.el ends here
