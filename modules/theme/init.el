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
                        '(font-lock-delimiter-face ((t (:inherit shadow))))
                        ;; Drop the pre/code-block background on light themes;
                        ;; the gfm-code-fences border replaces it visually.
                        '(markdown-pre-face ((((background light))
                                              (:background unspecified))))
                        '(markdown-code-face ((((background light))
                                               (:background unspecified)))))



;;; init.el ends here
