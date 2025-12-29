;;; init-format.el --- Code formatting -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Code formatting

;; Apply code formatting on save. Works for a range of languages.
(use-package apheleia :ensure t
  :after-call +first-file-hook
  :custom
  (apheleia-remote-algorithm 'local)
  (apheleia-formatters-respect-fill-column t)
  :config
  (apheleia-global-mode +1))

;; By default, trim trailing whitespace aggressively.

(defvar-local +trim-trailing-whitespace-aggressively t)

(add-hook! 'before-save-hook
  (when +trim-trailing-whitespace-aggressively
    (delete-trailing-whitespace)))

;; TODO: Evaluate whether ws-butler is something I need. Do I ever work in
;; codebases where I want to preserve existing trailing whitespace?

;; Delete trailing whitespace on visited lines.
(use-package ws-butler :ensure t :disabled t
  :hook (prog-mode-hook text-mode-hook conf-mode-hook)
  :config
  (pushnew! ws-butler-global-exempt-modes
            'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode))


(provide 'init-format)

;;; init-format.el ends here
