;;; mod-gptel.el --- Configuration for gptel -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'gptel)
(require 'gptel-anthropic)
(require 'general)

(cl-eval-when (compile)
  (require 'evil))

(general-def :keymaps 'gptel-mode-map :states '(normal insert)
  "C-c C-s" (defun +gptel-send ()
              (interactive)
              (unless (region-active-p)
                (goto-char (line-end-position)))
              (gptel-send)
              (evil-normal-state)))



(setq gptel-backend
      (gptel-make-anthropic "Claude"
        :stream t
        :key (lambda ()
               (auth-source-pick-first-password :host "api.anthropic.com"))))

(add-hook 'gptel-mode-hook 'visual-line-mode)

(with-eval-after-load 'evil
  (add-hook 'gptel-mode-hook 'evil-insert-state))


;;; Configure use of org-mode for gptel buffers

(setq gptel-default-mode 'org-mode)

(alist-set! gptel-prompt-prefix-alist 'org-mode "* ")

(setq-hook! 'gptel-mode-hook
  org-pretty-entities-include-sub-superscripts nil)

(add-hook 'gptel-mode-hook #'visual-line-mode)

(provide 'mod-gptel)

;;; mod-gptel.el ends here
