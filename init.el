;; -*- lexical-binding: t; -*-

(load-file (expand-file-name "./elpaca-bootstrap.el" user-emacs-directory))

(setq inhibit-startup-screen t)
(setq use-package-always-defer t)

(setq ring-bell-function #'ignore)

(server-start)


;;; General editing

(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq fill-column 80)

(use-package elec-pair
  ;; Automatically insert matching pairs.
  :init
  (electric-pair-mode +1))

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode . hs-minor-mode))


;;; evil-mode

(use-package evil :ensure (:wait t)
  ;; Evil is a better vim emulation implementation than the one that
  ;; ships with Emacs.
  :demand t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :custom
  (evil-symbol-word-search t)
  :config
  (evil-mode +1)

  :bind
  ;; Undefine useless forward-char binding.
  (:map evil-motion-state-map ("SPC" . nil)))

(add-hook 'emacs-lisp-mode-hook
          (defun +set-emacs-lisp-lookup-func ()
            (setq-local evil-lookup-func (defun +emacs-lisp-lookup-func ()
                                           (interactive)
                                           (describe-symbol (symbol-at-point))))))

(use-package evil-org :ensure t
  ;; Provides extra evil keybindings for org-mode, org-agenda etc.
  :hook (org-mode . evil-org-mode)
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :config
    (evil-org-agenda-set-keys))

  :custom
  (evil-v$-excludes-newline t))

(use-package evil-collection :ensure (:wait t)
  ;; Community-managed collection of evil keybindings; makes evil behave more
  ;; consistently across many modes.
  :after evil
  :demand t
  :config
  (evil-collection-init))

(use-package evil-surround :ensure t
  ;; Evil-surround makes the S key work as an operator to surround an
  ;; object with, e.g., matched parentheses.
  :after evil
  :demand t
  :config
  (global-evil-surround-mode +1))


;;; Completion

(use-package vertico :ensure (:wait t)
  ;; Vertico provides a better completion UI than the built-in default.
  :demand t
  :config

  :custom
  (vertico-preselect 'no-prompt)
  (vertico-cycle t)

  :bind (:map vertico-map
              ("C-<return>" . minibuffer-complete-and-exit))

  :config
  (vertico-mode +1)

  (use-package vertico-directory
    ;; Extension that teaches vertico how to operate on filename
    ;; components in a more ergonomic way.
    :demand t
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("C-l" . vertico-insert)
                ("C-h" . vertico-directory-delete-word)))

  (use-package vertico-repeat
    ;; Quickly restore the previous vertico command you ran.
    :hook (minibuffer-setup . vertico-repeat-save)
    :config
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))
    :bind
    (("C-x SPC" . vertico-repeat)
     :map vertico-map
     ("M-P" . vertico-repeat-previous)
     ("M-N" . vertico-repeat-next))))

(use-package marginalia :ensure t
  ;; Marginalia shows extra information alongside minibuffer items
  ;; during completion.
  :after vertico
  :demand t
  :config
  (marginalia-mode +1))

(use-package orderless :ensure t
  ;; Orderless allows you to filter completion candidates by typing
  ;; space-separated terms in any order.
  :after vertico
  :demand t
  :custom
  (completion-category-defaults nil)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  ;; Persists Emacs completion history. Used by vertico.
  :init (savehist-mode +1))

(setq enable-recursive-minibuffers t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq completion-ignore-case t)

;; Hide commands that don't work in the current major-mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(define-advice completing-read-multiple (:filter-args (args) crm-indicator)
  "Display the separator during `completing-read-multiple'."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 (rx (or (and bos "[" (*? any) "]*")
                         (and "[" (*? any) "]*" eos)))
                 ""
                 crm-separator)
                (car args))
        (cdr args)))


;;; VC & magit

(use-package transient :ensure t
  ;; Magit depends on a more recent version of transient than the one that ships
  ;; with Emacs.
  )

(use-package magit :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-mode-hook
            (defun +git-commit-initial-state ()
              (when (and (bound-and-true-p evil-mode)
                         (thing-at-point-looking-at (rx bol (* space) eol)))
                (evil-insert-state)))))

;; Don't prompt when following links to files that are under version control.
(setq vc-follow-symlinks t)

