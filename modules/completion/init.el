;;; completion/init.el --- Completion systems -*- lexical-binding: t; -*-

;;; Commentary:

;; Minibuffer and in-buffer completion framework using Vertico, Corfu, and Consult.

;;; Code:

(require '+corelib)

;; Vertico provides a better completion UI than the built-in default.
(use-package vertico  :hook +first-input-hook
  :custom
  (vertico-preselect 'no-prompt)
  (vertico-cycle t)
  :general-config (:keymaps 'vertico-map
                            "C-RET" #'vertico-exit-input
                            "RET" #'vertico-directory-enter
                            "DEL" #'vertico-directory-delete-char
                            "C-l" #'vertico-insert
                            "C-h" #'vertico-directory-delete-word
                            "M-l" #'vertico-insert
                            "M-h" #'vertico-directory-delete-word
                            "M-P" #'vertico-repeat-previous
                            "M-N" #'vertico-repeat-next)
  :init
  (vertico-mode +1)

  ;; Extension that teaches vertico how to operate on filename components in a
  ;; more ergonomic way.
  (use-package vertico-directory
    :demand t
    :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

  ;; Quickly restore the previous vertico command you ran.
  (use-package vertico-repeat
    :hook (minibuffer-setup-hook . vertico-repeat-save)
    :config
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))


;; Marginalia shows extra information alongside minibuffer items
;; during completion.
(use-package marginalia  :hook +first-input-hook
  :general
  (:keymaps 'minibuffer-local-map "M-A" #'marginalia-cycle))


;; Customise minibuffer completion behaviour.
;;
;; The configuration that determines which style to use is rather subtle; see
;; § 5.4.1:
;; https://protesilaos.com/emacs/dotemacs#h:14b09958-279e-4069-81e3-5a16c9b69892
;;
;; Briefly, use the following approach:
;;
;; 1. Prefer explicit and prefix matches first, falling back to orderless
;; matching last.
;;
;; 2. Override this behaviour explicitly for a few select types of completion.

(use-package minibuffer
  :custom
  ;; To determine a completion style when entering text in the minibuffer,
  ;; consult `completion-category-overrides' according to the type of thing
  ;; we're trying to complete. Fall back to `completion-styles' if there are no
  ;; specific style set for that type.
  ;;
  ;; Completion strategies are tried in order until a match is found. Putting
  ;; orderless last means more precise approaches are tried first.
  ;;
  ;; See `completion-styles-alist' for the behaviour of specific completion
  ;; styles.

  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless)))))

  (completion-styles '(basic substring initials flex orderless))

  ;; Disable any out-of-the-box defaults.
  (completion-category-defaults nil))


;; Filter completion candidates by typing
;; space-separated terms in any order.
(use-package orderless  :after-call +first-input-hook)


;; Persists Emacs completion history. Used by vertico.
(use-package savehist
  :init (savehist-mode +1)
  :custom
  (savehist-autosave-interval nil) ; on exit
  (history-delete-duplicates t)
  :functions (savehist-printable)
  :config
  (pushnew! savehist-additional-variables
            'kill-ring
            'register-alist
            'mark-ring 'global-mark-ring
            'search-ring 'regexp-search-ring)

  (setq-hook! 'savehist-save-hook
    ;; Reduce size of savehist's cache by dropping text properties.
    kill-ring (mapcar #'substring-no-properties (cl-remove-if-not #'stringp kill-ring))
    register-alist (cl-loop for (reg . item) in register-alist
                            if (stringp item)
                            collect (cons reg (substring-no-properties item))
                            else collect (cons reg item))

    ;; Avoid attempts to save unprintable registers, e.g. window configurations.
    register-alist (seq-filter #'savehist-printable register-alist)))


(setq enable-recursive-minibuffers t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)


;; Provides a variant of completing-read that allows users to enter multiple
;; values, separated by a delimiter.
(use-package crm
  :config
  (define-advice completing-read-multiple (:filter-args (args) crm-indicator)
    "Display the separator during `completing-read-multiple'."
    (let ((sans-brackets
           (replace-regexp-in-string (rx (or (and bos "[" (*? any) "]*")
                                              (and "[" (*? any) "]*" eos)))
                                     ""
                                     crm-separator)))
      (cons (format "[CRM %s] %s" (propertize sans-brackets 'face 'error) (car args))
            (cdr args)))))


;; Corfu provides in-buffer completions as you type.
(use-package corfu  :hook (+first-input-hook . global-corfu-mode)
  :general-config (:keymaps 'corfu-map
                            "RET" #'corfu-send
                            "<escape>" #'corfu-reset
                            "C-n" #'corfu-next
                            "C-p" #'corfu-previous)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.24)
  (corfu-quit-no-match t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-on-exact-match nil)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (tab-always-indent 'complete)
  (corfu-popupinfo-delay '(1.0 . 0.5))
  (global-corfu-modes '((not org-mode help-mode) t))
  :init
  (setq-hook! 'eshell-mode-hook corfu-auto nil)
  :config
  (corfu-popupinfo-mode +1)

  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))


;; Adds icons to corfu popups.
(use-package nerd-icons-corfu  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;; Adds useful functionality for `completion-at-point-functions'.
(use-package cape  :init
  (add-hook! 'prog-mode-hook
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (add-hook! 'org-mode-hook
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))


;; which-key displays a UI popup of available key commands as you type.
(use-package which-key
  :demand t
  :init
  (which-key-mode +1)
  :custom
  (which-key-prefix-prefix "…")
  (which-key-idle-delay 0.4)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  :config
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3))


;; Consult provides commands for common tasks that leverage the Emacs
;; completion system. It composes well with the above packages.
(use-package consult  :general
  ([remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap evil-show-registers]           #'consult-register
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap Info-search]                   #'consult-info
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop)

  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-preview-key "C-SPC")

  ;; Optimise for responsive input.
  (consult-async-min-input 2)
  (consult-async-refresh-delay  0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-fd-args
   '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
     "--color=never"
     ;; https://github.com/sharkdp/fd/issues/839
     "--full-path --absolute-path"
     "--hidden --exclude .git"))

  :config
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5))


;; Embark provides a UI for performing contextual actions on selected items
;; within completing-read.
(use-package embark  :general
  (:states '(normal emacs motion)
           "C-@" #'embark-act
           "C-t" #'embark-dwim)
  (:keymaps +default-minibuffer-maps
            "C-@" #'embark-act
            "C-c C-e" #'embark-export
            "C-c C-c" #'embark-collect))


;; Integrate embark with consult
(use-package embark-consult  :after (:any consult embark)
  :demand t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(pushnew! completion-ignored-extensions
          ".DS_Store"
          ".eln"
          ".drv"
          ".direnv/"
          ".git/"
          )


;; Set how the default option for empty input is displayed in the minibuffer.
(use-package minibuf-eldef
  :hook (after-init . minibuffer-electric-default-mode)
  :custom
  (minibuffer-default-prompt-format " [%s]"))


;; Dynamically complete using identifier-like words entered in this or other
;; buffers.
(use-package dabbrev
  :custom
  (dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (dabbrev-upcase-means-case-search t)
  :config
  (pushnew! dabbrev-ignored-buffer-modes
            'docview-mode 'pdf-view-mode))


(use-package find-func
  :custom
  (find-library-include-other-files nil))

(provide 'completion-init)

;;; init.el ends here
