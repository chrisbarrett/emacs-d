;; -*- lexical-binding: t; -*-

;; TODO: Remove once Emacs 30 is out of pretest.
(when (eq emacs-major-version 30)
  (setq elpaca-core-date 20241219))

(require 'elpaca-bootstrap
         (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

(setq inhibit-startup-screen t)
(setq use-package-always-defer t)

(setq ring-bell-function #'ignore)
(setq backups-inhibited t)

(require 'server)
(unless (server-running-p)
  (server-start))

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

(add-to-list 'trusted-content (expand-file-name (file-name-concat find-function-C-source-directory "../lisp/")))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "lisp/"))


;;; Macros

(defmacro pushnew! (var &rest elements)
  "Add missing ELEMENTS to VAR in-place."
  (let ((gvar (gensym)))
    `(let ((,gvar ',var))
       (set ,gvar
            (seq-union (eval ,gvar) ',elements)))))


;;; Leader key

(use-package general :ensure (:wait t) :demand t
  ;; General provides a featureful key binding system. It makes defining leader
  ;; key bindings much easier.
  :init
  (general-auto-unbind-keys)
  (general-unbind :states '(normal motion) "SPC")
  :config
  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   :prefix-command '+leader-key

   "SPC" #'consult-buffer
   "x" #'execute-extended-command
   "r" #'vertico-repeat
   ":" #'pp-eval-expression
   "d" #'dirvish
   "u" #'universal-argument-more
   "i" #'consult-imenu

   "/" #'consult-ripgrep
   "*" (defun +consult-ripgrep-symbol ()
         (interactive)
         (consult-ripgrep nil (format "%s" (symbol-at-point))))

   "<tab>" (defun +swap-buffers ()
             "Switch between the previous buffer and the current one."
             (interactive)
             (switch-to-buffer nil))

   "p"  '(nil :which-key "project")
   "p" project-prefix-map

   "h"  '(nil :which-key "help")
   "h" help-map

   "b"  '(nil :which-key "buffers")
   "bb" #'bury-buffer
   "bd" #'kill-current-buffer
   "bl" #'ibuffer-list-buffers

   "f"  '(nil :which-key "files")
   "ff" #'find-file
   "fF" #'find-file-other-window
   "fs" #'save-buffer
   "fr" #'recentf

   "fD" (defun +delete-file-and-buffer ()
          (interactive)
          (let ((file (buffer-file-name)))
            (kill-buffer (current-buffer))
            (when file
              (delete-file file))))

   "fy" (defun +copy-file-path ()
          (interactive)
          (if-let* ((file (buffer-file-name)))
              (progn
                (kill-new file)
                (message "Copied to clipboard => %s" file))
            (user-error "Buffer is not visiting a file")))

   "fd" (defun +copy-file-directory ()
          (interactive)
          (if-let* ((file (buffer-file-name))
                    (dir (file-name-directory file)))
              (progn
                (kill-new dir)
                (message "Copied to clipboard => %s" dir))
            (user-error "Buffer is not visiting a file")))

   "fv" (defun +revisit-file ()
          (interactive)
          (if-let* ((file (buffer-file-name)))
              (find-alternate-file file)
            (user-error "Buffer is not visiting a file")))

   "n"  '(nil :which-key "narrowing")
   "nf" #'narrow-to-defun
   "nr" #'narrow-to-region
   "nw" #'widen

   "c"  '(nil :which-key "comments")
   "cr" #'comment-dwim

   "g"  '(nil :which-key "git/goto")
   "gg" #'magit-status
   "g?" (defun +goto-messages ()
          (interactive)
          (display-buffer "*Messages*"))

   "gi" (defun +goto-init-el-file ()
          (interactive)
          (find-file (file-name-concat user-emacs-directory "init.el")))

   "gn" (defun +goto-nix-file ()
          (interactive)
          (project-find-file-in  "flake.nix" nil
                                 (project-current nil "~/.config/nix-configuration")))

   "o"  '(nil :which-key "org")
   "oi" (defun +goto-org-roam-index ()
          (interactive)
          (find-file (file-name-concat org-roam-directory "notes/index.org")))

   "oa" (defun +org-agenda-dwim ()
          (interactive)
          (require 'org)
          (require 'org-clock)
          (org-agenda nil (if (org-clocking-p) "w" "p")))

   "e"  '(nil :which-key "errors")
   "el" #'consult-flymake

   "w"  '(nil :which-key "windows")
   "wd" #'delete-window
   "wo" #'delete-other-windows
   "wq" #'delete-window
   "ww" #'other-window
   "w/" #'split-window-horizontally
   "w-" #'split-window-vertically
   ))


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
  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error))

(use-package dirvish :ensure t
  ;; Wrapper around `dired' that provides better UX.
  :general
  (:keymaps '(dirvish-mode-map dired-mode-map) :states 'normal
            "q" #'dirvish-quit)
  :init
  (dirvish-override-dired-mode))

(keymap-global-set "C-c SPC"
                   (defun +insert-nbsp ()
                     (interactive)
                     (insert-char #x00A0)))

(use-package winner
  ;; Provides undo/redo for buffer & window layout changes.
  :general-config (:keymaps 'winner-mode-map
                            "C-," #'winner-undo
                            "C-." #'winner-redo)
  :init
  (winner-mode +1)
  :config
  (with-eval-after-load 'evil
    (keymap-set evil-normal-state-map "C-." #'winner-redo)))

(use-package saveplace
  ;; Save buffer position
  :demand t
  :config (save-place-mode +1))

(use-package ws-butler :ensure t
  ;; Automatically remove trailing whitespace on edited lines.
  :hook (prog-mode text-mode))


;;; evil-mode

(use-package evil :ensure t
  ;; Evil is a better vim emulation implementation than the one that
  ;; ships with Emacs.
  :demand t
  :general-config (:states 'normal "M-." nil)
  :custom
  (evil-want-keybinding nil)
  (evil-want-integration t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-v$-excludes-newline t)
  :init
  (evil-mode +1))

(use-package vundo :ensure
  (vundo :host github :repo "casouri/vundo")
  ;; Visualise the Emacs undo history.
  :general ("C-x u" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package evil-collection :ensure t
  ;; Community-managed collection of evil keybindings; makes evil behave more
  ;; consistently across many modes.
  :after evil
  :demand t
  :custom
  ;; Ensure we do not overwrite the leader key binding.
  (evil-collection-key-blacklist '("SPC" "S-SPC"))
  :init
  (evil-collection-init)
  :config
  (define-advice evil-collection-magit-init (:after (&rest _) bind-leader)
    (general-define-key :keymaps evil-collection-magit-maps
                        :states '(normal)
                        "SPC" #'+leader-key)))

(use-package evil-surround :ensure t
  ;; Evil-surround makes the S key work as an operator to surround an
  ;; object with, e.g., matched parentheses.
  :hook ((text-mode prog-mode) . evil-surround-mode)
  ;; Use lowercase 's' for surround instead of 'S'.
  :general (:states '(visual) :keymaps 'evil-surround-mode-map "s" #'evil-surround-region)
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\{ . ("{" . "}"))
                               (?# . ("#{" . "}"))
                               (?> . ("<" . ">"))
                               (?f . evil-surround-function)
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)))

  :config
  (add-hook 'emacs-lisp-mode-hook
            (defun +elisp-configure-evil-surround ()
              (make-local-variable 'evil-surround-pairs-alist)
              (setf (alist-get ?` evil-surround-pairs-alist) '("`" . "'"))
              (setf (alist-get ?f evil-surround-pairs-alist) 'evil-surround-prefix-function))))


;;; Completion

(use-package vertico :ensure t
  ;; Vertico provides a better completion UI than the built-in default.
  :demand t
  :custom
  (vertico-preselect 'no-prompt)
  (vertico-cycle t)
  :general-config (:keymaps 'vertico-map
                            "C-<return>" #'minibuffer-complete-and-exit
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

  (use-package vertico-directory
    ;; Extension that teaches vertico how to operate on filename
    ;; components in a more ergonomic way.
    :demand t
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package vertico-repeat
    ;; Quickly restore the previous vertico command you ran.
    :hook (minibuffer-setup . vertico-repeat-save)
    :config
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))

(use-package marginalia :ensure t
  ;; Marginalia shows extra information alongside minibuffer items
  ;; during completion.
  :after vertico
  :demand t
  :init
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
  :general-config (:keymaps 'corfu-map "RET" #'corfu-send)
  :custom
  (corfu-auto t)
  (corfu-quit-no-match t)
  (tab-always-indent 'complete)
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :init
  (global-corfu-mode +1)
  (add-hook 'eshell-mode-hook (defun +corfu-eshell-setup ()
                                (setq-local corfu-auto nil)
                                (corfu-mode +1)))
  :config
  (corfu-popupinfo-mode +1))

(setq text-mode-ispell-word-completion nil)

(use-package which-key
  ;; which-key displays a UI popup of available key commands as you type.
  :demand t
  :init
  (which-key-mode +1)
  :custom
  (which-key-prefix-prefix "…")
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
  :general
  ("C-@" #'embark-act
   "M-." #'embark-dwim))

(use-package embark-consult :ensure t
  ;; Integration embark with consult
  :after (:any consult embark)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(pushnew! completion-ignored-extensions
          ".DS_Store"
          ".eln"
          ".drv"
          ".direnv/"
          ".git/"
          )


;;; VC & magit

(use-package transient :ensure t
  ;; Magit depends on a more recent version of transient than the one that ships
  ;; with Emacs.
  )

(use-package magit :ensure t
  ;; Magit is the definitive UX for working with git.
  :config
  (add-hook 'git-commit-mode-hook
            (defun +git-commit-initial-state ()
              (when (and (bound-and-true-p evil-mode)
                         (thing-at-point-looking-at (rx bol (* space) eol)))
                (evil-insert-state)))))

;; Don't prompt when following links to files that are under version control.
(setq vc-follow-symlinks t)

(pushnew! vc-directory-exclusion-list
          "node_modules"
          "cdk.out"
          "target"
          ".direnv"
          )


;;; projects

(use-package project
  ;; Emacs' built-in project lib
  :custom
  (project-switch-commands
   (defun +project-switch-magit-status ()
     (interactive)
     (let* ((proj (project-current t))
            (root (project-root proj)))
       (if (file-directory-p (file-name-concat root ".git"))
           (magit-status-setup-buffer root)
         (dirvish root)))))
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
  :general-config (:keymaps 'emacs-lisp-mode-map
                            "C-c RET" #'pp-macroexpand-last-sexp
                            "C-c C-c" #'+eval-dwim)
  :config
  (defun +eval-dwim (&optional beg end)
    (interactive (when (region-active-p)
                   (list beg end)))
    (if (and beg and)
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
(setq org-roam-directory "~/org/roam")

(use-package org
  :hook ((org-mode . abbrev-mode)
         (org-mode . auto-fill-mode))
  :custom
  (abbrev-file-name (file-name-concat org-directory "abbrev.el"))

  ;; visual settings
  (org-list-indent-offset 1)
  (org-cycle-separator-lines 1)
  (org-ellipsis " …")
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-folded 'showall)
  (org-startup-indented t)
  (org-startup-shrink-all-tables t)
  (org-startup-with-inline-images t)

  ;; TODOs, checkboxes, stats, properties.
  (org-todo-keywords '((type "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c@)")))
  (org-enforce-todo-dependencies t)
  (org-hierarchical-todo-statistics nil)
  (org-use-property-inheritance t)

  ;; babel & src support
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
  (org-babel-default-header-args:emacs-lisp '((:lexical . "yes")))
  (org-babel-python-command "python3")
  (org-babel-load-languages '((emacs-lisp . t)
                              (C . t)
                              (calc . t)
                              (shell . t)))

  ;; interactive behaviour
  (org-bookmark-names-plist nil)
  (org-M-RET-may-split-line nil)
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
  (org-footnote-auto-adjust t)
  (org-insert-heading-respect-content t)
  (org-loop-over-headlines-in-active-region 'start-level)

  :config

  ;; Prefer inserting headings with M-RET
  (add-hook 'org-metareturn-hook
            (defun +org-metareturn-append-line ()
              (when (org-in-item-p)
                (org-insert-heading current-prefix-arg)
                (evil-append-line 1)
                t)))

  ;; Automatically enter insert state when inserting new headings, logbook notes
  ;; or when using `org-capture'.

  (defun +org-enter-evil-insert-state (&rest _)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil))
      (evil-insert-state)))

  (dolist (cmd '(org-insert-heading
                 org-insert-heading-respect-content
                 org-insert-todo-heading-respect-content
                 org-insert-todo-heading))
    (advice-add cmd :after #'+org-enter-evil-insert-state))

  (define-advice org-capture (:after (&rest _) insert-state)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil)
               (bound-and-true-p org-capture-mode))
      (evil-insert-state)))

  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

  ;; Ensure we use dired rather than the Finder on macOS.

  (when (equal system-type 'darwin)
    (add-to-list 'org-file-apps '(directory . emacs)))

  ;; Don't show secondary selection when running `org-show-todo-tree'.
  (advice-add #'org-highlight-new-match :override #'ignore)
  )

(use-package org-habit
  :custom
  (org-habit-graph-column 72)
  (org-habit-today-glyph ?▲)
  (org-habit-completed-glyph ?✓))

(use-package evil-org :ensure t
  ;; Provides extra evil keybindings for org-mode, org-agenda etc.
  :hook (org-mode . evil-org-mode)
  :custom
  (evil-org-key-theme '(todo navigation insert textobjects additional calendar))
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :demand t
    :config
    (evil-org-agenda-set-keys)
    (evil-define-key 'motion org-agenda-mode-map
      (kbd "SPC") nil
      (kbd "/") #'org-agenda-filter)))

(use-package org-agenda
  :general
  ("C-c a" #'org-agenda)
  :config (require '+agenda)
  :custom
  (org-agenda-files (file-name-concat org-directory "org-agenda-files"))
  (org-agenda-text-search-extra-files `(agenda-archives ,(file-name-concat org-directory "archive.org")))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-search-view-always-boolean t)
  (org-archive-tag "ARCHIVED")
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
                           :max-gap 0)))))))
  :config

  ;; Use page-break separator for sections

  (setq org-agenda-block-separator (char-to-string ?\f))

  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'org-agenda-mode)

    (define-advice org-agenda (:after (&rest _) draw-separator)
      (page-break-lines--update-display-tables))

    (define-advice org-agenda-redo (:after (&rest _) draw-separator)
      (page-break-lines--update-display-tables)))

  ;; Search for and update agenda files automatically

  (defvar +org--agenda-update-process nil)

  (defconst +agenda-files-update-script
    (file-name-concat user-emacs-directory "scripts/update-agenda-files.sh"))

  (defun +org-agenda-update-files ()
    (interactive)
    (unless (and +org--agenda-update-process (process-live-p +org--agenda-update-process))
      (setq +org--agenda-update-process
            (start-process "update-org-agenda-files" nil +agenda-files-update-script))))

  (add-hook 'org-mode-hook
            (defun +update-org-agenda-files ()
              (add-hook 'after-save-hook
                        #'+org-agenda-update-files
                        nil
                        t)))

  ;; Forget deleted agenda files without prompting
  (define-advice org-check-agenda-file (:override (file) always-remove-missing)
    (unless (file-exists-p file)
      (org-remove-file file)
      (throw 'nextfile t)))

  ;; Reveal context around item on TAB
  (add-hook 'org-agenda-after-show-hook
            (defun +org-reveal-context ()
              (org-overview)
              (org-reveal)
              (org-fold-show-subtree)
              (org-display-outline-path)))
  )

(use-package org-roam :ensure t)


;;; Input methods

(setq default-input-method "french-postfix")

(with-eval-after-load "quail/latin-post"
  (require '+quail)

  (message "Initializing custom keybindings for latin-post")
  
  (+quail-defun "french-postfix" ";"
    (delete-horizontal-space)
    (insert " ; "))

  (+quail-defun "french-postfix" ":"
    (delete-horizontal-space)
    (let ((left-pad (cond
                     ((equal (char-before) ?:)
                      "")
                     ((and (derived-mode-p 'org-mode) (org-at-item-p) (not (org-at-item-description-p)))
                      " ")
                     (t
                      " "))))
      (insert left-pad ": "))))



;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
