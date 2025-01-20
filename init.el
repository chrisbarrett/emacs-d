;; -*- lexical-binding: t; -*-

;; TODO: Remove once Emacs 30 is out of pretest.
(when (eq emacs-major-version 30)
  (setq elpaca-core-date 20241219))

(require 'elpaca-bootstrap
         (expand-file-name "./elpaca-bootstrap.el" user-emacs-directory))

(setq inhibit-startup-screen t)
(setq use-package-always-defer t)

(setq ring-bell-function #'ignore)
(setq backups-inhibited t)

(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;; Leader key

(defvar-keymap +buffer-prefix-map
  "b" #'bury-buffer
  "d" #'kill-current-buffer
  "l" #'ibuffer-list-buffers)

(defvar-keymap +file-prefix-map
  "f" #'find-file
  "F" #'find-file-other-window
  "s" #'save-buffer
  "r" #'recentf

  "D" (defun +delete-file-and-buffer ()
        (interactive)
        (let ((file (buffer-file-name)))
          (kill-buffer (current-buffer))
          (when file
            (delete-file file))))

  "y" (defun +copy-file-path ()
        (interactive)
        (if-let* ((file (buffer-file-name)))
            (progn
              (kill-new file)
              (message "%s" file))
          (user-error "Buffer is not visiting a file")))

  "v" (defun +revisit-file ()
        (interactive)
        (if-let* ((file (buffer-file-name)))
            (find-alternate-file file)
          (user-error "Buffer is not visiting a file"))))

(defvar-keymap +narrowing-prefix-map
  "f" #'narrow-to-defun
  "r" #'narrow-to-region
  "w" #'widen)

(defvar-keymap +git/goto-prefix-map
  "s" #'magit-status

  "i" (defun +goto-init-el-file ()
        (interactive)
        (find-file (expand-file-name "init.el" user-emacs-directory)))

  "n" (defun +goto-nix-file ()
          (interactive)
          (project-find-file-in  "flake.nix" nil
                                 (project-current nil "~/.config/nix-configuration"))))

(defvar-keymap +org-prefix-map
  "a" (defun +org-agenda-dwim ()
        (interactive)
        (require 'org)
        (require 'org-clock)
        (org-agenda nil (if (org-clocking-p) "w" "p"))))

(defvar-keymap +errors-prefix-map
  "l" #'consult-flymake)

(defvar-keymap +windows-prefix-map
  "d" #'delete-window
  "o" #'delete-other-windows
  "q" #'delete-window
  "w" #'other-window
  "/" #'split-window-horizontally
  "-" #'split-window-vertically)

(defvar-keymap +leader-map
  :doc "Keymap for leader key (SPC)."
  "SPC" #'consult-buffer
  "x" #'execute-extended-command
  "r" #'vertico-repeat
  ":" #'pp-eval-expression
  "d" #'dirvish

  "/" #'consult-ripgrep
  "*" (defun +consult-ripgrep-symbol ()
        (interactive)
        (consult-ripgrep nil (format "%s" (symbol-at-point))))

  "b" +buffer-prefix-map
  "p" project-prefix-map
  "f" +file-prefix-map
  "n" +narrowing-prefix-map
  "g" +git/goto-prefix-map
  "o" +org-prefix-map
  "w" +windows-prefix-map
  "e" +errors-prefix-map
  "h" help-map
  "<tab>" (defun +swap-buffers ()
            "Switch between the previous buffer and the current one."
            (interactive)
            (switch-to-buffer nil)))

(add-hook 'evil-local-mode-hook (defun +bind-leader-key ()
                                  (evil-local-set-key 'motion (kbd "SPC") +leader-map)
                                  (evil-local-set-key 'normal (kbd "SPC") +leader-map)))


;;; Theme

(defun +theme-update ())


;;; General editing

(put 'narrow-to-region 'disabled nil)

(setq-default indent-tabs-mode nil)
(setq-default require-final-newline t)
(setq-default fill-column 80)

(use-package elec-pair
  ;; Automatically insert matching pairs.
  ;; TODO: replace with puni?
  :init
  (electric-pair-mode +1))

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode . hs-minor-mode))

(add-hook 'find-file-hook
          (defun +maybe-enable-readonly-mode ()
            (when (and (buffer-file-name)
                       (string-match-p (rx "/emacs/elpaca/") (buffer-file-name)))
              (read-only-mode +1))))

(use-package minions :ensure t
  ;; Hides minor modes, which are generally uninteresting and consume lots of
  ;; space.
  :demand t
  :config
  (minions-mode +1))

(use-package page-break-lines :ensure t
  ;; Displays ^L page break characters as a horizontal rule. Useful for
  ;; demarcating sections of a file.
  :demand t
  :config
  (global-page-break-lines-mode +1))

(use-package recentf
  ;; Maintain a list of visited files.
  :demand t
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package flymake
  ;; Frontend for in-buffer error checking & navigation.
  ;;
  ;; c.f. `next-error' and friends, which operate on compilation & grep results
  ;; across any number of buffers.
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package dirvish :ensure t
  ;; Wrapper around `dired' that provides better UX.
  )


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
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode +1)

  :bind
  (:map evil-normal-state-map ("M-." . nil))
  ;; Undefine useless forward-char binding.
  (:map evil-motion-state-map ("SPC" . nil)))

(use-package vundo :ensure
  (vundo :host github :repo "casouri/vundo")
  ;; Visualise the Emacs undo history.
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package evil-org :ensure t
  ;; Provides extra evil keybindings for org-mode, org-agenda etc.
  :hook (org-mode . evil-org-mode)
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :demand t
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
                ("C-h" . vertico-directory-delete-word)
                ("M-l" . vertico-insert)
                ("M-h" . vertico-directory-delete-word)))

  (use-package vertico-repeat
    ;; Quickly restore the previous vertico command you ran.
    :hook (minibuffer-setup . vertico-repeat-save)
    :config
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))
    :bind
    (:map vertico-map
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

(use-package corfu :ensure t
  ;; Corfu provides in-buffer completions as you type.
  :demand t
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (tab-always-indent 'complete)
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config
  (global-corfu-mode +1)
  (corfu-popupinfo-mode +1)

  (add-hook 'eshell-mode-hook (defun +corfu-eshell-setup ()
                                (setq-local corfu-auto nil)
                                (corfu-mode)))

  :bind
  (:map corfu-map ("RET" . corfu-send)))

(setq text-mode-ispell-word-completion nil)

(use-package which-key
  ;; which-key displays a UI popup of available key commands as you type.
  :demand t
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.4))

(use-package consult :ensure t
  ;; Consult provides commands for common tasks that leverage the Emacs
  ;; completion system. It composes well with the above packages.
  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  :config

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5))

(use-package embark :ensure t
  ;; Embark provides a UI for performing contextual actions on selected items
  ;; within completing-read.
  :bind
  (("C-@" . embark-act)
   ("M-." . embark-dwim))

  :init
  (use-package embark-consult :ensure t
    :after consult
    :demand t
    :hook (embark-collect-mode . consult-preview-at-point-mode)))


;;; VC & magit

(use-package transient :ensure t
  ;; Magit depends on a more recent version of transient than the one that ships
  ;; with Emacs.
  )

(use-package magit :ensure t
  :config
  (add-hook 'git-commit-mode-hook
            (defun +git-commit-initial-state ()
              (when (and (bound-and-true-p evil-mode)
                         (thing-at-point-looking-at (rx bol (* space) eol)))
                (evil-insert-state)))))

;; Don't prompt when following links to files that are under version control.
(setq vc-follow-symlinks t)


;;; projects

(use-package project
  ;; Emacs' built-in project lib
  :config
  (project-remember-projects-under "~/.config")
  (project-remember-projects-under "~/src")
  (project-remember-projects-under "~/org"))


;;; Documentation systems

(use-package help
  :custom
  (help-window-select t))


;;; Programming modes

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c RET" . pp-macroexpand-last-sexp)
              ("C-c C-c" . +eval-dwim))

  :config
  (defun +eval-dwim (&optional beg end)
    (interactive "r")
    (if (region-active-p)
        (message "Eval region => %s" (eval-region beg end))
      (message "Eval defun => %s" (eval-defun nil))))

  (add-hook 'emacs-lisp-mode-hook
            (defun +set-emacs-lisp-lookup-func ()
              (setq-local evil-lookup-func (defun +emacs-lisp-lookup-func ()
                                             (interactive)
                                             (describe-symbol (symbol-at-point)))))))

(use-package nix-ts-mode :ensure t
 :mode "\\.nix\\'")


;;; org-mode

(setq org-directory "~/org")

(use-package org
  :init
  (use-package org-habit :after org :demand t))

(use-package org-agenda
  :bind (("C-c a" . org-agenda))
  :config (require '+agenda)
  :custom
  (org-agenda-files (expand-file-name "org-agenda-files" org-directory))
  (org-agenda-custom-commands
   (let ((today '(agenda ""
                         ((org-agenda-overriding-header "Today")
                          (org-agenda-use-time-grid t)
                          (org-agenda-clockreport-parameter-plist '(:compact t
                                                                             :link t
                                                                             :maxlevel 3
                                                                             :fileskip0 t
                                                                             :filetitle t))
                          (org-agenda-skip-function #'+agenda-view-skip-function))))
         (next-actions '(tags-todo "-project-tickler-inbox+TODO=\"TODO\""
                                   ((org-agenda-overriding-header "Next Actions")
                                    (org-agenda-skip-function #'+agenda-next-actions-skip-function))))

         (inbox '(tags-todo "+inbox+TODO=\"TODO\""
                            ((org-agenda-overriding-header "Inbox"))))

         (delegated '(todo "WAIT"
                           ((org-agenda-overriding-header "Delegated")
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))

         (projects '(tags-todo "+project+TODO=\"TODO\""
                               ((org-agenda-overriding-header "Projects"))))

         (tickler
          '(todo "TODO"
                 ((org-agenda-overriding-header "Tickler")
                  (org-agenda-skip-function #'+agenda-tickler-section-skip-function))))


         (unprocessed-notes
          '(tags-todo "+outline-project+TODO=\"TODO\""
                      ((org-agenda-overriding-header "Unprocessed Notes")
                       (org-agenda-skip-function #'+agenda-next-actions-skip-function))))

         (defaults `((org-agenda-todo-ignore-scheduled 'future)
                     (org-habit-preceding-days 14)
                     (org-habit-following-days 7)
                     (org-agenda-span 'day)
                     (org-agenda-window-setup 'only-window)
                     (org-agenda-start-day nil)
                     (org-agenda-include-diary nil)
                     (org-agenda-insert-diary-extract-time nil)
                     (org-agenda-show-inherited-tags nil)
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-dim-blocked-tasks 'invisible)
                     (org-agenda-sorting-strategy '((agenda time-up category-up priority-down todo-state-up)
                                                    (todo priority-down category-up scheduled-up)
                                                    (tags priority-down category-up)
                                                    (search category-up)))
                     (org-agenda-clock-report-header "\nClocking")
                     (org-agenda-tags-column -100)
                     (org-agenda-use-time-grid nil)
                     (org-agenda-start-with-log-mode '(closed state))
                     (org-agenda-show-future-repeats nil)
                     (org-agenda-ignore-properties '(effort appt))
                     (org-agenda-archives-mode t))))

     `(("p" "personal agenda" ,(list today next-actions inbox delegated projects tickler)
        (,@defaults
         (org-agenda-tag-filter-preset '("-work" "-ignore"))))
       ("w" "work agenda" ,(list today next-actions inbox delegated projects tickler unprocessed-notes)
        (,@defaults
         (org-agenda-tag-filter-preset (list "-ignore" (format "+%s" (timekeep-work-tag))))
         (org-agenda-clock-consistency-checks
          '(:gap-ok-around ("12:20" "12:40" "4:00")
                           :max-duration "10:00"
                           :min-duration 0
                           :max-gap 0))))))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
