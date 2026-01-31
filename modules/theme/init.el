;;; init.el --- Theme module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Initializes theme configuration. Called after autoloads are registered.

;;; Code:

(require '+autoloads)

(use-package catppuccin-theme
  :demand t
  :init
  (setq +theme-dark 'catppuccin)
  (+theme-update))

;; Custom face overrides for consistency across themes.
(custom-theme-set-faces 'user
                        '(region ((((background light))
                                   (:inherit lazy-highlight))))
                        '(iedit-occurrence ((t (:inherit query-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))



;;; init.el ends here
