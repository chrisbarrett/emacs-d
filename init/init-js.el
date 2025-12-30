;;; init-js.el --- JavaScript & TypeScript -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package js
  :mode ("\\.[mc]?js" . js-ts-mode))

(use-package typescript-ts-mode
  :hook (typescript-ts-mode-hook . eglot-ensure)
  :init
  ;; Activate on shebangs for node, deno, bun etc.
  (add-to-list 'magic-mode-alist (cons (rx bol "#!" (+? nonl) (or "/" " ") (or "deno" "node" "bun" "tsx")
                                           (* space) (or eol (and "--" (+ nonl))))
                                       'typescript-ts-mode))
  :config
  (use-package mod-typescript :demand t))

(provide 'init-js)

;;; init-js.el ends here
