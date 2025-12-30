;;; init-eglot.el --- LSP configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package flymake
  ;; Frontend for in-buffer error checking & navigation.
  ;;
  ;; c.f. `next-error' and friends, which operate on compilation & grep results
  ;; across any number of buffers.
  :hook (prog-mode-hook . flymake-mode)
  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error))


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
  (eglot-code-action-indicator "")

  ;; Make RET open markdown links in the eldoc buffer.
  ;;
  ;; https://github.com/joaotavora/eglot/discussions/1238
  :config
  (defun +eglot-open-link ()
    (interactive)
    (if-let* ((url (get-text-property (point) 'help-echo)))
        (browse-url url)
      (user-error "No URL at point")))

  (define-advice eldoc-display-in-buffer (:after (&rest _) update-keymap)
    (with-current-buffer eldoc--doc-buffer
      (general-define-key :keymaps 'local :states 'motion "RET" #'+eglot-open-link)))

  ;; Prevent display of inlay hints
  (add-hook 'eglot-managed-mode-hook
            (defun +eglot-inlay-hints-off ()
              (eglot-inlay-hints-mode -1))))


;; Teach eglot to use lsp-booster for better performance.
(use-package eglot-booster :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :demand t
  :config (eglot-booster-mode +1))


(provide 'init-eglot)

;;; init-eglot.el ends here
