;;; init-reader.el --- Document reader -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; General-purpose document reader. Uses native modules with buffered
;; rendering to improve performance.
(use-package reader ; native deps--package injected via Nix
  :init
  (require 'reader-autoloads))


(provide 'init-reader)

;;; init-reader.el ends here
