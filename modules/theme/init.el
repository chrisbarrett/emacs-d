;;; init.el --- Theme module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Initializes theme configuration. Called after autoloads are registered.

;;; Code:

;; Custom face overrides for consistency across themes.
(custom-theme-set-faces 'user
                        '(region ((((background light))
                                   (:inherit lazy-highlight))))
                        '(iedit-occurrence ((t (:inherit query-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))

;; Set catppuccin as the dark theme and apply.
(setq +theme-dark 'catppuccin)
(+theme-update)

(provide 'theme-init)

;;; init.el ends here
