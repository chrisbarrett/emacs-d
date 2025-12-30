;;; init-zig.el --- Zig language -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package zig-mode :ensure t
  :mode "\\.\\(zig\\|zon\\)\\'"
  :custom
  (zig-format-on-save nil) ; use apheleia instead.
  )


(provide 'init-zig)

;;; init-zig.el ends here
