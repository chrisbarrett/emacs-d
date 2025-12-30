;;; init-ui.el --- General editor UI -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Tune scrolling behaviour
(setq hscroll-margin 2)
(setq hscroll-step 1)
(setq scroll-conservatively 10)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
(setq delete-pair-blink-delay 0.1)

(setq use-dialog-box nil)

(setq next-error-recenter '(4))

;; Show keystrokes in minibuffer pretty much immediately.
(setq echo-keystrokes 0.02)


;; Disable bidirectional text by default.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't render cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)


;; Emacs' built-in tooltip system. Just disable the thing.
(use-package tooltip
  :init (tooltip-mode -1))


;; Silence "For information about GNU Emacs and the GNU system..." on startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Don't tell me what key I could have used instead of M-x.
(advice-add #'execute-extended-command--describe-binding-msg :override #'ignore)


;; Emacs' built-in tab-bar. I use it pretty much just use it for git
;; worktrees.
(use-package tab-bar
  :custom
  (tab-bar-close-button-show 'selected)
  (tab-bar-auto-width-max '((270) 25))
  :general
  ("M-S-."  #'tab-bar-switch-to-next-tab
   "M->"    #'tab-bar-switch-to-next-tab
   "M-S-,"  #'tab-bar-switch-to-prev-tab
   "M-<"    #'tab-bar-switch-to-prev-tab
   "M-C-," #'tab-bar-move-tab-backward
   "M-C-." #'tab-bar-move-tab)
  :init
  (tab-bar-mode +1)
  :config
  (delq! 'tab-bar-format-add-tab tab-bar-format)

  (custom-theme-set-faces 'user
                          '(tab-bar-tab ((t (:bold t)))))

  (use-package mod-tabs
    :demand t
    :general (:keymaps 'override-global-map "M-B" #'+tabs-menu)))


;; Teach Emacs how to display ligatures when available.
(use-package ligature :ensure t
  :after-call +first-buffer-hook +first-file-hook
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode (+read-eld "ligatures/prog-mode.eld"))
  (ligature-set-ligatures 'compilation-mode (+read-eld "ligatures/prog-mode.eld"))
  (ligature-set-ligatures '(text-mode org-agenda-mode) (+read-eld "ligatures/text-mode.eld"))

  (global-ligature-mode t))


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

  ;; SAFETY: Functions provided via autoloads.
  (with-no-warnings
    (dimmer-configure-which-key)
    (dimmer-configure-magit)
    (dimmer-configure-org)
    (dimmer-configure-hydra)
    (dimmer-configure-posframe))

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
  (eval-and-compile
    (define-advice show-paren--show-context-in-overlay (:after (&rest _) format-overlay)
      (let ((current-text (overlay-get show-paren--context-overlay 'display)))
        (overlay-put show-paren--context-overlay 'display
                     (string-pad (concat "↑ " current-text)
                                 (window-width)
                                 (string-to-char " "))))
      (overlay-put show-paren--context-overlay
                   'face `(:box (:line-width (0 . 1) :color ,(face-attribute 'shadow :foreground)))))))


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


;; A better buffer list than the default.
(use-package bufler :ensure t
  :config
  (use-package mod-bufler :demand t))


;; Turns URLs in the buffer into clickable buttons.
(use-package goto-addr
  :init
  (defun +goto-address-maybe-h ()
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (goto-address)
      (goto-address-mode +1)))
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook magit-process-mode-hook) . +goto-address-maybe-h))


;; Improve performance of files with very long lines.
(use-package so-long
  :hook (elpaca-after-init-hook . global-so-long-mode))


;; User-process management UI.
(use-package proced
  :custom
  (proced-enable-color-flag t))


;; Window management stuff that's not in the C layer.
(use-package window
  :general (:keymaps 'override-global-map "M-o" #'other-window)

  ;; Prefer vertical splits--better when the Emacs GUI window is wide rather
  ;; than tall.
  :custom
  (split-width-threshold 160)
  (split-height-threshold nil))


(provide 'init-ui)

;;; init-ui.el ends here
