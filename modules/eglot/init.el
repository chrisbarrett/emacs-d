;;; init.el --- Eglot module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; LSP client configuration with flymake integration and performance booster.

;;; Code:

(require '+corelib)

;; Frontend for in-buffer error checking & navigation.
;;
;; See also: `next-error' and friends, which operate on compilation & grep
;; results across any number of buffers.
(use-package flymake
  :hook (prog-mode-hook . flymake-mode)

  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error)

  :config
  ;; Display full diagnostic in echo area for fallthrough destination.
  (alist-set! flymake-diagnostic-format-alist t '(origin code message)))


;; Emacs' built-in LSP integration.
(use-package eglot

  :general-config
  (:keymaps 'eglot-mode-map
   :states '(insert normal)
   "M-RET" #'eglot-code-actions)
  (:keymaps 'eglot-mode-map
   :states '(normal)
   "C-c C-r" #'eglot-rename)

  :custom
  (eglot-code-action-indicator "")

  ;; Make RET open markdown links in the eldoc buffer.
  ;;
  ;; https://github.com/joaotavora/eglot/discussions/1238

  :config
  (define-advice eldoc-display-in-buffer (:after (&rest _) update-keymap)
    (with-current-buffer eldoc--doc-buffer
      (general-define-key :keymaps 'local :states 'motion "RET" #'+eglot-open-link)))

  ;; Prevent display of inlay hints
  (add-hook 'eglot-managed-mode-hook #'+eglot-inlay-hints-off))


;; Teach eglot to use lsp-booster for better performance.
(use-package eglot-booster
  :after eglot
  :demand t
  :config (eglot-booster-mode +1))


(provide 'eglot-init)

;;; init.el ends here
