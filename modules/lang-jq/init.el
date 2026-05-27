;;; lang-jq/init.el --- jq filter script support -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for jq filter scripts via `jq-mode' plus a JSON-buffer
;; keybinding for live filter evaluation.

;;; Code:

(require '+autoloads)
(require '+corelib)

(use-package jq-mode
  :mode "\\.jq\\'"
  :config
  (with-eval-after-load 'json-mode
    (general-def :keymaps 'json-mode-map "C-c C-j" #'jq-interactively))
  (with-eval-after-load 'json-ts-mode
    (general-def :keymaps 'json-ts-mode-map "C-c C-j" #'jq-interactively)))

;;; init.el ends here
