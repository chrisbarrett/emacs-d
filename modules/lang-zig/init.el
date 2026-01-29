;;; init.el --- Zig language module -*- lexical-binding: t; -*-

;;; Commentary:

;; Zig language support with zig-mode.  Format-on-save is disabled in favor
;; of apheleia which provides consistent formatting across all languages.

;;; Code:

(require '+autoloads)

(use-package zig-mode :ensure nil
  :mode "\\.\\(zig\\|zon\\)\\'"
  :custom
  (zig-format-on-save nil))



;;; init.el ends here
