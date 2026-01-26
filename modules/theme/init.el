;;; init.el --- Theme module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Initializes theme configuration. Called after autoloads are registered.
;;
;; Note: The actual theme application is handled by init-theme.el using
;; use-package to ensure the catppuccin-theme package is installed first.
;; This module provides the library functions and face customizations.

;;; Code:

;; Custom face overrides for consistency across themes.
(custom-theme-set-faces 'user
                        '(region ((((background light))
                                   (:inherit lazy-highlight))))
                        '(iedit-occurrence ((t (:inherit query-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))

(provide 'theme-init)

;;; init.el ends here
