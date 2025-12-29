;;; init-ui.el --- General editor UI -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package catppuccin-theme :ensure (:wait t) :demand t
  :init
  (setq +theme-dark 'catppuccin)
  (+theme-update))


;; Basic code folding.
(use-package hideshow
  :hook (prog-mode-hook . hs-minor-mode))


;; Displays ^L page break characters as a horizontal rule. Useful for
;; demarcating sections of a file.
(use-package page-break-lines :ensure t
  :after-call +first-file-hook +first-buffer-hook
  :config
  (global-page-break-lines-mode +1)
  (pushnew! page-break-lines-modes 'rfc-mode 'prog-mode 'text-mode))


;; Disable the mode-line in situations where it's not useful.
(use-package hide-mode-line :ensure (hide-mode-line
                                     :host github
                                     :repo "hlissner/emacs-hide-mode-line")
  :hook ((completion-list-mode-hook
          Man-mode-hook
          ielm-mode-hook
          magit-setup-buffer-hook
          calendar-mode-hook
          eshell-mode-hook
          compilation-mode-hook
          help-mode-hook
          shell-command-mode-hook
          eat-mode-hook
          org-roam-mode-hook
          )
         . hide-mode-line-mode)

  :init
  (defvar-local +hide-modeline-was-enabled-p nil)

  (add-hook '+side-window-raised-hook
            (defun +side-window--show-modeline-when-raised ()
              (setq +hide-modeline-was-enabled-p hide-mode-line-mode)
              (hide-mode-line-mode -1)))

  (add-hook '+side-window-returned-hook
            (defun +side-window--hide-mode-line-on-return ()
              (hide-mode-line-mode (if +hide-modeline-was-enabled-p
                                       +1
                                     -1)))))


;; Ensure numbers always have syntax highlighting applied, even if a
;; major-mode neglects to configure that.
(use-package highlight-numbers :ensure (highlight-numbers
                                        :host github
                                        :repo "Fanael/highlight-numbers")
  :hook (prog-mode-hook conf-mode-hook)
  :custom (highlight-numbers-generic-regexp
           (rx symbol-start (+ digit) (? "." (* digit)) symbol-end)))


;; Show line-numbers in the margin.
(use-package display-line-numbers
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))


;; Display TODO comments with special highlights.
(use-package hl-todo :ensure t
  :hook (prog-mode-hook yaml-ts-mode-hook conf-mode-hook)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("NOTE" success bold)
     ("SAFETY" success bold))))


;; Display indentation guides in buffers. Particularly useful for
;; indentation-sensitive language modes.
(use-package indent-bars :ensure t
  :hook (yaml-ts-mode-hook python-ts-mode-hook)
  :custom
  (indent-bars-starting-column 0)
  (indent-bars-width-frac 0.15)
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-highlight-current-depth nil))


;; Temporarily highlights the current line after performing certain operations
(use-package pulsar :ensure t
  :hook (+first-input-hook . pulsar-global-mode)
  :custom
  (pulsar-iterations 5)
  (pulsar-pulse-on-window-change t)
  :config
  (use-package mod-pulsar :demand t))


;; Dim inactive windows to highlight the active window
(use-package dimmer :ensure t
  :hook (+first-input-hook . dimmer-mode)
  :custom
  (dimmer-adjustment-mode :both)
  (dimmer-fraction -0.05)
  (dimmer-use-colorspace :cielab)
  (dimmer-watch-frame-focus-events t)
  :config
  ;; Configure integrations with popular packages
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-hydra)
  (dimmer-configure-posframe)
  (dimmer-mode t))


;; Highlight the current line.
(use-package hl-line
  :custom
  (hl-line-sticky-flag nil))


;; Visualise whitespace characters.
(use-package whitespace
  :config
  (delq! 'newline whitespace-style)
  (delq! 'newline-mark whitespace-style))


;; Provides `show-paren-mode', which highlights the matched pair at point.
(use-package paren
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay)
  :config
  (define-advice show-paren--show-context-in-overlay (:after (&rest _) format-overlay)
    (let ((current-text (overlay-get show-paren--context-overlay 'display)))
      (overlay-put show-paren--context-overlay 'display
                   (string-pad (concat "↑ " current-text)
                               (window-width)
                               (string-to-char " "))))
    (overlay-put show-paren--context-overlay
                 'face `(:box (:line-width (0 . 1) :color ,(face-attribute 'shadow :foreground))))))


;; Adds a face for parentheses in lisps. I hijack it to dim semicolons and
;; other non-critical syntax elements in other langs.
(use-package paren-face :ensure t
  :hook (lisp-data-mode-hook c-ts-base-mode-hook elixir-ts-mode-hook)

  :config
  (setq-hook! 'elixir-ts-mode-hook
    paren-face-regexp (rx (any "(),&")))

  (setq-hook! 'c-ts-base-mode-hook
    paren-face-regexp (rx (any ";,"))))


(use-package breadcrumb :ensure t
  :custom
  (breadcrumb-idle-time 0.3))


(provide 'init-ui)

;;; init-ui.el ends here
