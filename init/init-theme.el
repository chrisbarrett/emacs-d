;;; init-theme.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-theme-set-faces 'user
                        '(region ((((background light))
                                   (:inherit lazy-highlight))))
                        '(iedit-occurrence ((t (:inherit query-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))



;; Dark theme.
(use-package catppuccin-theme :ensure (:wait t) :demand t
  :init
  (setq +theme-dark 'catppuccin)
  (+theme-update))


(provide 'init-theme)

;;; init-theme.el ends here
