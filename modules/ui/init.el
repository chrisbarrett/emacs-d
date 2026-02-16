;;; init.el --- UI module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; General editor UI: scrolling, tab bar, ligatures, visual feedback,
;; modeline, display-buffer rules.

;;; Code:

(require '+autoloads)

(require '+corelib)

;;; Scrolling behaviour

(setq hscroll-margin 2)
(setq hscroll-step 1)
(setq scroll-conservatively 10)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)


;;; Cursor & Selection

(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)
(setq delete-pair-blink-delay 0.1)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)


;;; Bidirectional text

(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)


;;; Dialog & keystrokes

(setq use-dialog-box nil)
(setq echo-keystrokes 0.02)
(setq next-error-recenter '(4))


;;; Window splitting

(setq split-width-threshold 160)
(setq split-height-threshold nil)


;;; Tooltips

(use-package tooltip
  :init (tooltip-mode -1))


;;; Startup silence

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'execute-extended-command--describe-binding-msg :override #'ignore)


;;; Tab bar

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
  (setq tab-bar-format (delq 'tab-bar-format-add-tab tab-bar-format))

  (custom-theme-set-faces 'user
                          '(tab-bar-tab ((t (:bold t)))))

  ;; FIXME: This is LLM slop
  (with-eval-after-load 'transient
    (+update-tab-bar-themes)
    (setq tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)
    (add-hook '+theme-changed-hook #'+update-tab-bar-themes)
    (define-advice tab-bar-select-tab (:after (&rest _) schedule-alert)
      (+tab-bar--pulse-tab-switch)
      (+tab-bar--schedule-alert-clear))
    (add-hook 'tab-bar-tab-post-open-functions #'+tab-bar--pulse-new-tab)
    (add-hook 'tab-bar-tab-pre-close-functions #'+tab-bar--cleanup-timers-on-close)
    (add-hook 'delete-frame-functions #'+tab-bar--cleanup-timers-on-delete-frame))

  (general-def :keymaps 'override-global-map "M-B" #'+tabs-menu))


;;; Ligatures

(use-package ligature
  :after-call +first-buffer-hook +first-file-hook
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode (+read-eld "ligatures/prog-mode.eld"))
  (ligature-set-ligatures 'compilation-mode (+read-eld "ligatures/prog-mode.eld"))
  (ligature-set-ligatures '(text-mode org-agenda-mode) (+read-eld "ligatures/text-mode.eld"))
  (global-ligature-mode t))


;;; Code folding

(use-package hideshow
  :hook (prog-mode-hook . hs-minor-mode))


;;; Page breaks

(use-package page-break-lines
  :after-call +first-file-hook +first-buffer-hook
  :config
  (global-page-break-lines-mode +1)
  (pushnew! page-break-lines-modes 'rfc-mode 'prog-mode 'text-mode 'compilation-mode))


;;; Hide mode line

(use-package hide-mode-line
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
          org-roam-mode-hook)
         . hide-mode-line-mode)
  :preface
  (defvar-local +hide-modeline-was-enabled-p nil)
  :init

  (add-hook '+side-window-raised-hook
            (defun +side-window--show-modeline-when-raised ()
              (setq +hide-modeline-was-enabled-p hide-mode-line-mode)
              (hide-mode-line-mode -1)))

  (add-hook '+side-window-returned-hook
            (defun +side-window--hide-mode-line-on-return ()
              (hide-mode-line-mode (if +hide-modeline-was-enabled-p +1 -1)))))


;;; Number highlighting

(use-package highlight-numbers
  :hook (prog-mode-hook conf-mode-hook)
  :custom (highlight-numbers-generic-regexp
           (rx symbol-start (+ digit) (? "." (* digit)) symbol-end)))


;;; Line numbers

(use-package display-line-numbers
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))


;;; TODO highlighting

(use-package hl-todo
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


;;; Indentation guides

(use-package indent-bars
  :hook (yaml-ts-mode-hook python-ts-mode-hook)
  :custom
  (indent-bars-starting-column 0)
  (indent-bars-width-frac 0.15)
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-highlight-current-depth nil))


;;; Pulsar (visual feedback)

(use-package pulsar
  :hook (+first-input-hook . pulsar-global-mode)
  :custom
  (pulsar-iterations 5)
  (pulsar-pulse-on-window-change t)
  :config
  (+load "./config/+pulsar.el")
  (with-eval-after-load 'avy
    (+load "./config/+avy-pulsar.el")))


;;; Dimmer

(use-package dimmer
  :hook (+first-input-hook . dimmer-mode)
  :custom
  (dimmer-adjustment-mode :both)
  (dimmer-fraction -0.05)
  (dimmer-use-colorspace :cielab)
  (dimmer-watch-frame-focus-events t)
  :config
  (with-no-warnings
    (dimmer-configure-which-key)
    (dimmer-configure-magit)
    (dimmer-configure-org)
    (dimmer-configure-hydra)
    (dimmer-configure-posframe))
  (dimmer-mode t))


;;; Current line

(use-package hl-line
  :custom
  (hl-line-sticky-flag nil))


;;; Whitespace

(use-package whitespace
  :config
  (setq whitespace-style (delq 'newline (delq 'newline-mark whitespace-style))))


;;; Paren matching

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


;;; Paren face

(use-package paren-face
  :hook (lisp-data-mode-hook c-ts-base-mode-hook elixir-ts-mode-hook)
  :config
  (setq-hook! 'elixir-ts-mode-hook
    paren-face-regexp (rx (any "(),&")))
  (setq-hook! 'c-ts-base-mode-hook
    paren-face-regexp (rx (any ";,"))))


;;; Breadcrumb

(use-package breadcrumb
  :custom
  (breadcrumb-idle-time 0.3))


;;; Bufler

(use-package bufler
  :config
  (+load "./config/+bufler.el"))


;;; Clickable URLs

(use-package goto-addr
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook magit-process-mode-hook) . +goto-address-maybe-h))


;;; Long lines

(use-package so-long
  :hook (elpaca-after-init-hook . global-so-long-mode))


;;; Process list

(use-package proced
  :custom
  (proced-enable-color-flag t))


;;; Window keybindings

(use-package window
  :general (:keymaps 'override-global-map
                     "M-o" #'other-window
                     "M-_" #'window-toggle-side-windows))


;;; Modeline (doom-modeline)

(use-package doom-modeline
  :hook elpaca-after-init-hook
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-check-simple-format t)
  (doom-modeline-modal nil)

  :functions doom-modeline-set-modeline
  :preface
  (autoload 'hide-mode-line-mode "hide-mode-line")
  :config
  (add-hook 'magit-mode-hook
            (defun +modeline-hide-in-non-status-buffer-h ()
              "Show minimal modeline in magit-status buffer, no modeline elsewhere."
              (if (eq major-mode 'magit-status-mode)
                  (doom-modeline-set-modeline 'magit)
                (hide-mode-line-mode)))))


;;; Search count (anzu)

(use-package anzu
  :after-call isearch-mode)

(use-package evil-anzu
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))


;;; Display buffer rules

(use-package display-buffer-alist-setup
  :no-require t
  :init
  (setq display-buffer-alist
        (cl-labels ((mode-active-p (mode)
                      (cl-assert (symbolp mode))
                      (lambda (buf _action)
                        (with-current-buffer buf
                          (and (boundp mode) (eval mode))))))
          (append

           ;; Full-frame
           (cl-labels ((full-frame (pred &rest overrides)
                         (cons pred `((display-buffer-full-frame)
                                      ,@overrides))))
             (list
              (full-frame (rx bos "CAPTURE-"))))

           ;; Top side
           (cl-labels ((top (pred &rest overrides)
                         (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                      ,@overrides
                                      (dedicated . t)
                                      (window-height . 0.2)
                                      (window-width . 80)
                                      (side . top)
                                      (slot . 0)))))
             (list
              (top '(or (derived-mode . debugger-mode)
                        (derived-mode . profiler-report-mode)))
              (top (rx bos "CAPTURE-") '(window-height . 0.6))))

           ;; Left side
           (cl-labels ((left (pred &rest overrides)
                         (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                      ,@overrides
                                      (dedicated . t)
                                      (window-height . 0.3)
                                      (window-width . 80)
                                      (side . left)
                                      (slot . 0)))))
             (list
              (left (rx bos "*Debugger-record*" eos)
                    '(slot . 1)
                    '(window-height . 0.3))
              (left `(or
                      (derived-mode . grep-mode)
                      (derived-mode . occur-mode)
                      (derived-mode . embark-collect-mode)
                      ,(rx bos "*Embark Export: ")
                      ,(rx bos "*org-roam-search"))
                    '(window-width . 80))))

           ;; Right side
           (cl-labels ((right (pred &rest overrides)
                         (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                      ,@overrides
                                      (dedicated . t)
                                      (window-height . 0.3)
                                      (window-width . 80)
                                      (side . right)
                                      (slot . 0)))))
             (list
              (right `(or
                       ,(rx bos "*shell command output*" eos)
                       ,(rx bos "*Org babel results*" eos)
                       ,(rx bos "*async shell command*" eos)))
              (right `(or
                       (derived-mode . rfc-mode)
                       (derived-mode . help-mode)
                       (derived-mode . helpful-mode)
                       (derived-mode . Man-mode)
                       (derived-mode . woman-mode)
                       (derived-mode . magit-log-mode)
                       ,(rx bos "*eldoc*" eos)
                       ,(rx bos "*org-roam*" eos)
                       ,(rx bos "*org-roam-links*" eos)
                       ,(rx bos "magit-process: "))
                     '(window-width . 80))))

           ;; Bottom
           (cl-labels ((bottom (pred &rest overrides)
                         (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                      ,@overrides
                                      (window-height . 0.3)
                                      (window-width . 80)
                                      (side . bottom)
                                      (dedicated . t)
                                      (slot . 0)))))
             (list
              (bottom `(or
                        (derived-mode . comint-mode)
                        (derived-mode . eat-mode)
                        (derived-mode . eshell-mode)
                        (derived-mode . compilation-mode)
                        ,(rx bos "*calendar*" eos)
                        ,(rx bos " *Agenda Commands*" eos)
                        ,(rx bos "*Org Select*" eos)
                        ,(rx bos "*Org Note*" eos)
                        ,(rx bos "*bd-new-issue*" eos)
                        ,(rx bos "*Org-Babel Error Output*" eos)
                        (derived-mode . ert-simple-view-mode)
                        ,(rx bol "*envrc*" eos)))))

           ;; Suppressed buffers
           (cl-labels ((suppress (pred)
                         (cons pred
                               `((display-buffer-no-window)
                                 (allow-no-window . t)))))
             (list
              (suppress `(or ,(rx bos "*warnings*" eos)
                             ,(rx bos "*async-native-compile-Log*" eos))))))))

  ;; Fallback action
  (setq display-buffer-fallback-action
        `((display-buffer--maybe-same-window
           display-buffer-reuse-window
           +display-buffer-reuse-non-dedicated-window
           display-buffer--maybe-pop-up-window
           display-buffer-in-previous-window
           display-buffer-use-some-window
           +display-buffer-fallback)))

  (setq window-combination-resize t)
  (setq switch-to-buffer-in-dedicated-window 'pop))

;;; init.el ends here
