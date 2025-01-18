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
  
  :config
  (evil-mode +1))

(use-package evil-org :ensure t
  ;; Provides extra evil keybindings for org-mode, org-agenda etc.
  :hook (org-mode . evil-org-mode)
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :config
    (evil-org-agenda-set-keys)))

(use-package evil-collection :ensure (:wait t)
  ;; Community-managed collection of evil keybindings; makes evil behave more
  ;; consistently across many modes.
  :after evil
  :config
  (evil-collection-init)

  (with-eval-after-load 'magit
    (evil-collection-magit-setup)))

(use-package evil-surround :ensure t
  :after evil
  :demand t
  :config
  (global-evil-surround-mode +1))


;;; Completion

(use-package vertico :ensure (:wait t)
  ;; Vertico provides a better completion UI than the built-in default.
  :demand t
  :init
  (vertico-mode +1))

(use-package orderless :ensure t
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

