;;; init.el --- main Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the primary Emacs init file, loaded by the editor on startup. It is
;; loaded after early-init.el.
;;
;; TODO:
;;
;; - something like smartparens
;;     - maybe puni?
;;
;; - file templates
;;
;; - text snippets

;;; Code:

(require 'use-package)

(eval-and-compile
  (require '+corelib (file-name-concat user-emacs-directory "lisp/+corelib.el")))

;; TODO: Remove once Emacs 30 is out of pretest.
(when (eq emacs-major-version 30)
  (defvar elpaca-core-date 20241219))

(require 'elpaca-bootstrap
         (file-name-concat user-emacs-directory "elpaca-bootstrap.el"))

(require 'server)
(unless (server-running-p)
  (server-start))

(eval-and-compile
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp")))

(add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "lisp/"))

(use-package find-func
  :config
  (add-to-list 'trusted-content (file-name-concat find-function-C-source-directory "../lisp/")))

;; Make sure I don't accidentally start loading super-expensive packages on startup.

(defconst +expensive-packages '(org org-roam org-agenda))

(add-hook 'after-init-hook
          (defun +assert-packages-deferred ()
            (when-let* ((loaded (seq-filter #'featurep +expensive-packages)))
              (warn "The following package(s) were loaded eagerly, rather than deferred: %S" loaded)))
          nil
          -99)


;;; Extra UI lifecycle hooks
;;
;; These are cribbed from Doom.

(defvar +switch-buffer-hook nil)
(defvar +switch-frame-hook nil)
(defvar +switch-window-hook nil)

(defun +run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks '+switch-buffer-hook)))

(defun +run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks '+switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks '+switch-window-hook))))

(add-hook 'after-init-hook
          (defun +install-ui-hooks-h ()
            (add-hook 'window-selection-change-functions #'+run-switch-window-or-frame-hooks-h)
            (add-hook 'window-buffer-change-functions #'+run-switch-buffer-hooks-h)
            (add-hook 'server-visit-hook #'+run-switch-buffer-hooks-h)))


;;; Leader key

(use-package general :ensure (:wait t) :demand t
  ;; General provides a featureful key binding system. It makes defining leader
  ;; key bindings much easier.
  :init
  (general-auto-unbind-keys)
  (general-unbind :states '(normal motion) "SPC")
  :config
  (require '+window)
  (require '+roam)

  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   :prefix-command '+leader-key

   "SPC" #'consult-buffer
   "x" #'execute-extended-command
   "r" #'vertico-repeat
   ":" #'pp-eval-expression
   ";" #'ielm
   "d" #'dirvish
   "u" #'universal-argument
   "i" #'consult-imenu
   "-" #'window-toggle-side-windows
   "!" #'async-shell-command

   "'" (general-predicate-dispatch #'poporg-dwim

         ;; Exit indirect edit session if active

         (bound-and-true-p poporg-mode) #'poporg-edit-exit
         (bound-and-true-p edit-indirect--overlay) #'edit-indirect-commit
         (bound-and-true-p org-src-mode) #'org-edit-src-exit

         ;; Otherwise, open indirect-edit buffer

         (and (derived-mode-p 'prog-mode)
              ;; Are we in a string or comment? See: `parse-partial-sexp'
              (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss))))
         #'poporg-dwim

         (and (derived-mode-p 'prog-mode) (region-active-p)) #'edit-indirect-region
         (equal (buffer-name) "*Edit Formulas*") #'org-table-fedit-finish
         (derived-mode-p 'org-mode) #'org-edit-special
         (and (derived-mode-p 'markdown-mode) (markdown-code-block-at-point-p))'markdown-edit-code-block)

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

   "a"  '(nil :which-key "apps")
   "ac" #'quick-calc
   "aC" #'full-calc
   "ae" #'eshell
   "ar" (general-predicate-dispatch 'profiler-start
          (and (featurep 'profiler) (profiler-running-p)) #'+profiler-stop-and-report)

   "ap"  '(nil :which-key "elpaca")
   "app" #'elpaca-manager
   "apl" #'elpaca-log
   "api" #'elpaca-info
   "apb" #'elpaca-browse
   "apv" #'elpaca-visit

   "b"  '(nil :which-key "buffers")
   "bb" #'bury-buffer
   "bd" #'kill-current-buffer
   "bl" #'ibuffer
   "bn" #'next-buffer
   "bp" #'previous-buffer

   "f"  '(nil :which-key "files")
   "ff" #'find-file
   "fF" #'find-file-other-window
   "fs" #'save-buffer
   "fR" #'rename-visited-file
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
   "cl" #'comment-line

   "g"  '(nil :which-key "git/goto")
   "gg" #'magit-status
   "gl" #'magit-log-buffer-file

   "g?" (defun +goto-messages ()
          (interactive)
          (display-buffer "*Messages*"))

   "ge" (defun +goto-emacs-config-file ()
          (interactive)
          (project-find-file-in "init.el" nil
                                (project-current nil user-emacs-directory)))

   "gn" (defun +goto-nix-file ()
          (interactive)
          (project-find-file-in  "flake.nix" nil
                                 (project-current nil "~/.config/nix-configuration")))

   "gp" (defun +goto-project-file ()
          (interactive)
          (let ((proj (project-current nil (project-prompt-project-dir))))
            (project-find-file-in nil nil proj)))

   "o"  '(nil :which-key "org")
   "oi" (defun +goto-org-roam-index ()
          (interactive)
          (require 'org-roam)
          (find-file (file-name-concat org-roam-directory "notes/index.org")))

   "oa" (defun +org-agenda-dwim ()
          (interactive)
          (require 'org)
          (require 'org-clock)
          (org-agenda nil (if (org-clocking-p) "w" "p")))
   "og" #'org-capture-goto-last-stored
   "ov" #'org-tags-view
   "ok" #'org-capture
   "ol" #'org-store-link
   "of" #'+roam-node-find
   "orr" #'org-roam-review
   "orl" #'org-roam-links
   "os" #'org-roam-search

   "e"  '(nil :which-key "errors")
   "el" #'consult-flymake

   "kr" #'consult-yank-pop

   "t"  '(nil :which-key "toggles")
   "td" #'dirvish-side
   "tf" #'global-display-fill-column-indicator-mode
   "ti" #'indent-bars-mode
   "tl" #'global-display-line-numbers-mode
   "tm" #'toggle-input-method
   "ts" #'spell-fu-mode
   "tr" #'read-only-mode

   "w"  '(nil :which-key "windows")
   "w-" #'+split-window-vertically-dwim
   "w/" #'+split-window-horizontally-dwim
   "w="  #'balance-windows
   "wd" #'delete-window
   "wo"  #'+delete-nondedicated-windows
   "wO"  #'delete-other-windows
   "wq" #'delete-window
   "wr" #'evil-window-rotate-downwards
   "ws" #'consult-register
   "wS" 'window-configuration-to-register
   "wt"  #'+toggle-window-dedication
   "ww" #'other-window
   ))

(defmacro +local-leader-set-key (keymaps &rest general-args)
  `(general-define-key :prefix "," :states '(normal motion) :keymaps ,keymaps ,@general-args))


;;; General editing

(put 'narrow-to-region 'disabled nil)

(setq-default fill-column 80)
(setq ring-bell-function #'ignore)

(setq create-lockfiles nil)
(setq auto-save-include-big-deletions t)

;; Wrap words on word boundaries.
(setq-default word-wrap t)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

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

(setq use-dialog-box nil)

;; Show keystrokes in minibuffer pretty much immediately.
(setq echo-keystrokes 0.02)

(use-package tooltip
  ;; Emacs' built-in tooltip system. Just disable the thing.
  :init (tooltip-mode -1))

(use-package simple
  ;; Core editing functionality.
  :custom
  (kill-do-not-save-duplicates t)
  ;; Hide commands that don't work in the current major-mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (setq-default indent-tabs-mode nil))

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(keymap-set minibuffer-local-map "C-p" #'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-n" #'next-line-or-history-element)

(use-package window
  ;; Window management stuff that's not in the C layer.

  ;; Prefer vertical splits--better when the Emacs GUI window is wide rather
  ;; than tall.
  :custom
  (split-width-threshold 160)
  (split-height-threshold nil))

(use-package frame
  ;; Frame management settings
  :custom
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :init
  (window-divider-mode +1))

(use-package paragraphs
  ;; Emacs' core paragraph parser.
  :custom
  (sentence-end-double-space nil))

(use-package files
  ;; General built-in file IO.
  :custom
  (require-final-newline t)
  (find-file-visit-truename t)
  (make-backup-files nil)
  (confirm-nonexistent-file-or-buffer nil)
  (auto-mode-case-fold nil)
  (version-control nil)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  (backup-directory-alist (list (cons "." (file-name-concat user-emacs-directory "backup/"))))

  :config
  (define-advice after-find-file (:around (fn &rest args) dont-block-on-autosave-exists)
    "Prevent the editor blocking to inform you when an autosave file exists."
    (cl-letf (((symbol-function #'sit-for) #'ignore))
      (apply fn args))))

(use-package startup
  :custom
  (auto-save-list-file-prefix (file-name-concat user-emacs-directory "autosave/"))
  :config
  (setq auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t))))

(use-package tramp
  ;; Provides remote editing support, e.g. over SSH connections.
  :after files
  :config
  (setq tramp-backup-directory-alist backup-directory-alist)
  (setq tramp-auto-save-directory (file-name-concat user-emacs-directory "tramp-autosave/")))

(use-package uniquify
  ;; Controls how buffers with conflicting names are managed.
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package autorevert
  ;; Automatically revert buffers.
  ;;
  ;; This configuration is adapted from Doom; it disables the file watcher
  ;; mechanism and instead auto-reverts based on users switching windows &
  ;; buffers. This is much less resource-intensive.
  :config
  (defun +auto-revert-current-buffer-h ()
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun +auto-revert-visible-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (+visible-buffers))
      (with-current-buffer buf
			   (+auto-revert-current-buffer-h))))
  :hook
  (after-save . +auto-revert-visible-buffers-h)
  (+switch-buffer . +auto-revert-current-buffer-h)
  (+switch-window . +auto-revert-current-buffer-h)
  :config
  (add-function :after after-focus-change-function #'+auto-revert-visible-buffers-h)

  :custom
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list ".")))

(use-package elec-pair
  ;; Automatically insert matching pairs.
  ;; TODO: replace with puni?
  :init
  (electric-pair-mode +1))

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode . hs-minor-mode))

(add-hook 'find-file-hook
          (defun +maybe-enable-readonly-mode-h ()
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
  :after-call recentf consult-buffer
  :defer-incrementally t
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(use-package paren
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package flymake
  ;; Frontend for in-buffer error checking & navigation.
  ;;
  ;; c.f. `next-error' and friends, which operate on compilation & grep results
  ;; across any number of buffers.
  :hook (prog-mode . flymake-mode)
  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error))

(use-package dired
  :custom
  (dired-listing-switches
   "--almost-all --human-readable --group-directories-first --no-group"))

(use-package dirvish :ensure t
  ;; Wrapper around `dired' that provides better UX.
  :general
  (:keymaps '(dirvish-mode-map dired-mode-map) :states 'normal
            "q" #'dirvish-quit)
  (:keymaps 'dirvish-mode-map :states 'normal
            "<tab>" #'dirvish-layout-toggle)
  :custom
  (dirvish-attributes
   '(vc-state subtree-state nerd-icons collapse file-size file-time))

  :preface
  (use-package nerd-icons :ensure t
    :demand t
    :after dirvish
    :config
    (setq dirvish-path-separators (list
                                   (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                   (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                   (format " %s " (nerd-icons-faicon "nf-fa-angle_right")))))
  :config
  (dirvish-peek-mode +1)
  
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
  :custom
  (winner-boring-buffers '("*Completions*" "*Compile-Log*" "*inferior-lisp*"
                           "*Fuzzy Completions*" "*Apropos*" "*Help*" "*cvs*"
                           "*Buffer List*" "*Ibuffer*" "*esh command on file*"))
  :config
  (with-eval-after-load 'evil
    (keymap-set evil-normal-state-map "C-." #'winner-redo)))

(use-package saveplace
  ;; Save buffer position when re-visiting files, even across Emacs sessions.
  :init (save-place-mode +1)

  :config
  (define-advice save-place-find-file-hook (:after-while (&rest _) recenter)
    "Recenter on cursor when loading a saved place."
    (when buffer-file-name (ignore-errors (recenter))))

  (define-advice save-place-alist-to-file (:around (fn &rest args) use-prin1-not-pp)
    "Use the faster prin1 for saving history."
    (cl-letf (((symbol-function #'pp) #'prin1))
           (apply fn args))))

(use-package ediff
  ;; File diff UI.
  :custom
  (ediff-diff-options "-w")
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-show-clashes-only t)
  :config
  (with-eval-after-load 'org
    (defun +ad-ediff-reveal-org-content-around-hunk (&rest _)
      (dolist (buf (list ediff-buffer-A ediff-buffer-B ediff-buffer-C))
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-reveal t))))))

    (advice-add 'ediff-next-difference :after #'+ad-ediff-reveal-org-content-around-hunk)
    (advice-add 'ediff-previous-difference :after #'+ad-ediff-reveal-org-content-around-hunk)))

(use-package tabify
  ;; Tab-to-space conversion
  :custom
  (tabify-regexp "^\t* [ \t]+"))

(use-package comint
  ;; Emacs' basic system for hosting interactive command interpreters.
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048) ; double the default.
  )

(use-package compile
  ;; Integration for running compilers and other processes from inside Emacs.
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil) ; automatically save before compiling.
  (compilation-scroll-output 'first-error)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; Automatically truncate long compilation bufers.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package hide-mode-line :ensure
  (hide-mode-line :host github :repo "hlissner/emacs-hide-mode-line")
  ;; Disable the mode-line in situations where it's not useful.
  :hook ((completion-list-mode Man-mode) . hide-mode-line-mode))

(use-package highlight-numbers :ensure
  (highlight-numbers :host github :repo "Fanael/highlight-numbers")
  ;; Ensure numbers always have syntax highlighting applied, even if a
  ;; major-mode neglects to configure that.
  :hook (prog-mode conf-mode)
  :custom (highlight-numbers-generic-regexp
           (rx symbol-start (+ digit) (? "." (* digit)) symbol-end)))

(use-package display-line-numbers
  ;; Show line-numbers in the margin.
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))

(use-package ws-butler :ensure t
  ;; Delete trailing whitespace on visited lines.
  :hook (prog-mode text-mode conf-mode)
  :config
  (pushnew! ws-butler-global-exempt-modes
            'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode))

;; Disable bidirectional text by default.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't render cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

(use-package spell-fu :ensure t
  ;; A more lightweight spell-checker than the built-in.
  :hook (text-mode prog-mode conf-mode)
  :config
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_AU"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "fr"))

  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))

  (setq-hook! 'org-mode-hook
    spell-fu-faces-exclude '(org-meta-line org-link org-code org-block
                             org-block-begin-line org-block-end-line
                             org-footnote)))

(use-package hl-todo :ensure t
  ;; Display TODO comments with special highlights.
  :hook (prog-mode yaml-ts-mode conf-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold))))

(use-package indent-bars :ensure t
  ;; Display indentation guides in buffers. Particularly useful for
  ;; indentation-sensitive language modes.
  :hook (yaml-ts-mode python-ts-mode)
  :custom
  (indent-bars-starting-column 0)
  (indent-bars-width-frac 0.15)
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-highlight-current-depth nil))

(use-package elpaca
  ;; Configure aspects of elpaca not required for initial package bootstrap.
  :general-config
  (:states 'normal
   :keymaps 'elpaca-manager-mode-map
   "/" #'elpaca-ui-search))

;; Teach Emacs that C-i and C-m do in fact exist.
(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   input-decode-map fallback
   (lambda (&rest _args)
     (interactive)
     (if (when-let ((keys (this-single-command-raw-keys)))
           (and (display-graphic-p)
                (not (cl-loop for event in events
                              if (cl-position event keys)
                              return t))
                ;; Use FALLBACK if nothing is bound to KEY, otherwise we've
                ;; broken all pre-existing FALLBACK keybinds.
                (key-binding (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                                      key)
                             nil t)))
         key fallback))))


;;; evil-mode

(use-package evil :ensure t
  ;; Evil is a better vim emulation implementation than the one that
  ;; ships with Emacs.
  :demand t
  :general-config
  (:states 'emacs "ESC ESC" #'evil-normal-state)
  :custom
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-v$-excludes-newline t)
  (evil-want-C-g-bindings)
  (evil-want-C-u-delete t)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  ;; Cursor customisation
  :init
  (custom-theme-set-faces 'user
                          `(cursor
                            ((((background dark))
                              (:background "#51afef"))
                             (((background light))
                              (:background "#000000")))))

  (defun +sync-evil-cursor-colors-with-theme ()
    (put 'cursor 'evil-emacs-color  (face-foreground 'warning))
    (put 'cursor 'evil-normal-color (face-background 'cursor)))

  (advice-add '+theme-update :after #'+sync-evil-cursor-colors-with-theme)
  (add-hook 'modus-themes-post-load-hook #'+sync-evil-cursor-colors-with-theme)
  (+sync-evil-cursor-colors-with-theme)

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  :config
  (setq evil-default-cursor '+evil-default-cursor-fn)
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn))
  (setq evil-insert-state-cursor 'bar)
  (setq evil-visual-state-cursor 'hollow)

  :config
  ;; Keep shift-width in sync if mode changes.
  (setq-hook! 'after-change-major-mode
    evil-shift-width tab-width)

  :config
  (add-hook '+escape-hook
            (defun +evil-disable-ex-highlights-h ()
              (when (evil-ex-hl-active-p 'evil-ex-search)
                (evil-ex-nohighlight)
                t)))

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
    (general-define-key :keymaps (append evil-collection-magit-maps
                                         evil-collection-magit-section-maps)
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
            (defun +elisp-configure-evil-surround-h ()
              (make-local-variable 'evil-surround-pairs-alist)
              (setf (alist-get ?` evil-surround-pairs-alist) '("`" . "'"))
              (setf (alist-get ?f evil-surround-pairs-alist) 'evil-surround-prefix-function))))

(use-package evil-goggles :ensure t
  ;; evil-goggles displays text highlights for changed regions.
  ;;
  ;; cf. volatile-highlights or goggles for non-evil configurations.
  :init
  (evil-goggles-mode +1)
  :custom
  (evil-goggles-duration 0.1)
  (evil-goggles-pulse nil)
  (evil-goggles-enable-delete nil)
  (evil-goggles-enable-change nil))

;; Adapt the escape key customisation from Doom.

(defvar +escape-hook nil
  "Hook functions run until success when ESC is pressed.")

(defun +escape (&optional interactive)
  "Run `+escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           ;; quit the minibuffer if open.
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          ((run-hook-with-args-until-success '+escape-hook))
          ;; don't abort macros
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ;; Back to the default
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'+escape)
(keymap-set minibuffer-mode-map "<escape>" #'+escape)

(use-package evil-multiedit :ensure t
  ;; Evil-compatible multiple cursors.
  :after evil
  :config
  (evil-multiedit-default-keybinds)

  :init
  (defun +multiedit ()
    (interactive)
    (evil-normal-state)
    (unless (eolp)
      (forward-char -1))
    (evil-multiedit-match-all))

  :general
  (:states 'visual
           "v" (general-predicate-dispatch #'evil-multiedit-match-all
                 (equal last-command 'evil-visual-char) #'+multiedit)))


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
  :init (savehist-mode +1)
  :custom
  (savehist-autosave-interval nil) ; on exit
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

(use-package crm
  ;; Provides a variant of completing-read that allows users to enter multiple
  ;; values, separated by a delimiter.
  :config
  (define-advice completing-read-multiple (:filter-args (args) crm-indicator)
    "Display the separator during `completing-read-multiple'."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   (rx (or (and bos "[" (*? any) "]*")
                           (and "[" (*? any) "]*" eos)))
                   ""
                   crm-separator)
                  (car args))
          (cdr args))))

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
  (setq-hook! 'eshell-mode-hook corfu-auto nil)
  (add-hook 'eshell-mode-hook (corfu-mode +1))
  :config
  (corfu-popupinfo-mode +1))

(use-package which-key
  ;; which-key displays a UI popup of available key commands as you type.
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
  (add-hook 'which-key-init-buffer-hook
            (defun +set-which-key-buffer-spacing-h ()
              (setq-local line-spacing 3))))

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
  (:states 'normal "M-." #'embark-dwim)
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
            (defun +git-commit-initial-state-h ()
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

  :autoload project-remember-projects-under
  :config
  (project-remember-projects-under "~/.config")
  (project-remember-projects-under "~/src")
  (project-remember-projects-under "~/org"))


;;; Documentation systems

(use-package help
  :custom
  (help-window-select t))

(use-package eldoc
  ;; Display help hints in the echo area as you move around.
  :config
  ;; Teach eldoc to re-run after these commands.
  (eldoc-add-command '+escape
                     'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))


;;; Text & programming modes

(use-package text-mode
  ;; Emacs' general parent mode for non-programming-language text files.

  :mode  ("/LICENSE\\'")

  ;; Not sure of the performance impact of this... leave off for now.
  ;;
  ;; :hook (text-mode . visual-line-mode)

  :custom
  (text-mode-ispell-word-completion nil))

(use-package elisp-mode
  :general-config (:keymaps 'emacs-lisp-mode-map "C-c RET" #'pp-macroexpand-last-sexp)

  :config
  (defun +emacs-lisp-lookup-func ()
    (describe-symbol (symbol-at-point)))

  (+local-leader-set-key 'emacs-lisp-mode-map
    "e" '(nil :which-key "eval")
    "eb" #'eval-buffer)

  (setq-hook! 'emacs-lisp-mode-hook
    evil-lookup-func #'+emacs-lisp-lookup-func)

  :init
  (use-package checkdoc
    :custom
    (checkdoc-force-docstrings-flag nil))

  (use-package +elisp
    :general (:keymaps 'emacs-lisp-mode-map "C-c C-c" #'+elisp-eval-dwim)

    ;; Improve plist indentation
    :autoload +elisp--calculate-lisp-indent-a
    :init
    (advice-add #'calculate-lisp-indent :override #'+elisp--calculate-lisp-indent-a)))

(use-package nix-ts-mode :ensure t
  :mode "\\.nix\\'")

(use-package hexl
  :general-config
  (:states 'normal
   :keymaps 'hexl-mode-map
   "h" #'hexl-backward-char
   "l" #'hexl-forward-char
   "]]" #'hexl-end-of-1k-page
   "[[" #'hexl-beginning-of-1k-page
   "$" #'hexl-end-of-line
   "^" #'hexl-beginning-of-line
   "0" #'hexl-beginning-of-line))

(use-package conf-mode
  ;; Unix configuration files

  :init
  ;; Fall back to conf-mode for rc files.
  (add-to-list 'auto-mode-alist (rx "rc" eos) 'append))


;;; org-mode


(use-package org :ensure t ; NB. installed from org package archive.
  ;; org-mode - the reason why I can probably never switch to another editor.

  ;; org is a chonker; decompose the load process into smaller features so it's
  ;; less noticeable.
  :defer-incrementally
  calendar find-func format-spec org-macs org-compat org-faces org-entities
  org-list org-pcomplete org-src org-footnote org-macro ob org org-habit org-agenda
  org-capture

  :hook ((org-mode . abbrev-mode)
         (org-mode . auto-fill-mode))

  :preface
  (setq org-directory "~/org")
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
  (org-log-into-drawer t)

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
  (org-return-follows-link t)

  :config

  ;; Prefer inserting headings with M-RET
  (add-hook 'org-metareturn-hook
            (defun +org-metareturn-append-line-h ()
              (when (org-in-item-p)
                (org-insert-heading current-prefix-arg)
                (evil-append-line 1)
                t)))

  ;; Automatically enter insert state when inserting new headings, logbook notes
  ;; or when using `org-capture'.

  :preface
  (defun +ad-org-enter-evil-insert-state (&rest _)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil))
      (evil-insert-state)))
  :config
  (dolist (cmd '(org-insert-heading
                 org-insert-heading-respect-content
                 org-insert-todo-heading-respect-content
                 org-insert-todo-heading))
    (advice-add cmd :after #'+ad-org-enter-evil-insert-state))

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
  :functions org-highlight-new-match
  :config (advice-add #'org-highlight-new-match :override #'ignore)

  ;; Make C-c C-k either cut subtrees or cancel open notes.
  :config
  (defun +org-cut-subtree-or-cancel-note ()
    (interactive)
    (if org-finish-function
        (org-finish-function
	 (let ((org-note-abort t)) (funcall org-finish-function)))
      (org-cut-subtree)))


  :general-config
  (:keymaps 'org-mode-map
            "C-c C-k" #'+org-cut-subtree-or-cancel-note
            "M-p" #'org-metaup
            "M-n" #'org-metadown)
  )

(use-package org-habit
  :after org-agenda
  :demand t
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
  (org-agenda-inhibit-startup t)
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
            (defun +update-org-agenda-files-h ()
              (add-hook 'after-save-hook #'+org-agenda-update-files nil t)))

  ;; Forget deleted agenda files without prompting
  (define-advice org-check-agenda-file (:override (file) always-remove-missing)
    (unless (file-exists-p file)
      (org-remove-file file)
      (throw 'nextfile t)))

  ;; Reveal context around item on TAB
  (add-hook 'org-agenda-after-show-hook
            (defun +org-reveal-context-h ()
              (org-overview)
              (org-reveal)
              (org-fold-show-subtree)
              (org-display-outline-path)))
  )

(use-package org-roam :ensure t
  :preface
  (setq org-roam-directory "~/org/roam")

  :general-config
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'org-roam-node-insert)

  :config
  (org-roam-db-autosync-mode +1)

  (+local-leader-set-key 'org-mode-map
    "<tab>" #'org-roam-buffer-toggle)

  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))
  (org-roam-extract-new-file-path "notes/${slug}.org"))

(use-package poporg :ensure t)


;;; Input methods

(setq default-input-method "french-postfix")

(with-eval-after-load "quail/latin-post"
  (eval-and-compile
    (require '+quail))

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



(use-package nursery :ensure
  (nursery :host github :repo "chrisbarrett/nursery"
           :files (:defaults "extensions/*"))

  :init
  (use-package org-roam-review
    :commands (org-roam-review org-roam-review-list-recently-added)
    :general-config
    (:states '(normal) :keymaps 'org-roam-review-mode-map
             ;; "TAB" 'magit-section-cycle
             "g r" 'org-roam-review-refresh))

  (use-package org-roam-search
    :commands (org-roam-search))

  (use-package org-roam-links
    :commands (org-roam-links))

  (use-package org-roam-dblocks
    :hook (org-mode . org-roam-dblocks-autoupdate-mode))

  (use-package org-roam-slipbox
    :after org-roam
    :demand t
    :config
    (org-roam-slipbox-buffer-identification-mode +1)
    (org-roam-slipbox-tag-mode +1)))



;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
