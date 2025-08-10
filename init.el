;;; init.el --- main Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the primary Emacs init file, loaded by the editor on startup. It is
;; loaded after early-init.el.

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

(require 'use-package)

(eval-and-compile
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp/"))
  (require '+corelib)
  (require '+load-incrementally))

(defvar org-directory "~/org/")
(defvar org-roam-directory "~/org/roam/")
(defvar org-default-notes-file "~/org/notes.org")

(defvar +site-files-directory (file-name-concat user-emacs-directory "site/"))
(defvar +templates-dir (file-name-concat user-emacs-directory "templates/"))


;;; Bootstrap Elpaca

(unless (featurep 'elpaca)
  (load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el")))

(with-no-warnings
  (elpaca elpaca-use-package
    (elpaca-use-package-mode)))

;; Make sure I don't accidentally start loading super-expensive packages on startup.

(defconst +expensive-packages '(org org-roam org-agenda forge))

(add-transient-hook! 'after-init-hook
  (when-let* ((loaded (seq-filter #'featurep +expensive-packages)))
    (warn "The following package(s) were loaded eagerly, rather than deferred: %S" loaded)))

(add-hook 'elpaca-after-init-hook #'+load-packages-incrementally-h)


;;; Extra UI & session lifecycle hooks

;; These hooks are cribbed from Doom--they're a great pattern for deferred
;; loading.

;; The first set are *transient* hooks; they are run only once in the Emacs
;; session, the first time a particular action is performed by the user.

(defvar +first-input-hook nil
  "Transient hook before first user input.")

(defvar +first-file-hook nil
  "Transient hook before first interactively opened file.")

(defvar +first-buffer-hook nil
  "Transient hook before first interactively opened buffer.")

;; These are set up to run just once by other hooks.

(+run-hook-once '+first-buffer-hook '(+switch-buffer-hook find-file-hook))
(+run-hook-once '+first-file-hook '(find-file-hook dired-initial-position-hook))
(+run-hook-once '+first-input-hook '(pre-command-hook))

;; The remaining hooks are executed routinely throughout the session.

(defvar +switch-buffer-hook nil
  "Hooks run after changing the current buffer.")

(defvar +switch-frame-hook nil
  "Hooks run after changing focused frame.")

(defvar +switch-window-hook nil
  "Hooks run after changing focused window.")

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

(add-transient-hook! 'after-init-hook
  (add-hook 'window-selection-change-functions #'+run-switch-window-or-frame-hooks-h)
  (add-hook 'window-buffer-change-functions #'+run-switch-buffer-hooks-h)
  (add-hook 'server-visit-hook #'+run-switch-buffer-hooks-h))



(use-package no-littering :ensure (:wait t) :demand t
  :config
  (no-littering-theme-backups))

(use-package general :ensure (:wait t) :demand t
  ;; General provides a featureful key binding system. It makes defining leader
  ;; key bindings much easier.
  :config
  (use-package mod-leader :demand t))


;; Adapt the escape key customisation from Doom.

(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map))

(general-def :keymaps +default-minibuffer-maps "S-v" #'yank)

(defvar +escape-hook nil
  "Hook functions run until success when ESC is pressed.")

(defun +escape (&optional interactive)
  "Quit things, abort things, and finish things.
Runs `+escape-hook'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t)
        (in-minibuffer? (minibuffer-window-active-p (minibuffer-window))))
    (cond
     (in-minibuffer?
      (when interactive (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))

     ;; Run all escape hooks. If any returns non-nil, then stop there.
     ((run-hook-with-args-until-success '+escape-hook))

     ;; Don't abort keyboard macros.
     ((or defining-kbd-macro executing-kbd-macro))

     ;; Fall back to keyboard-quit.
     (t
      (unwind-protect (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit)))))))

(global-set-key [remap keyboard-quit] #'+escape)
(global-set-key [remap abort-recursive-edit] #'+escape)

(general-def :keymaps +default-minibuffer-maps [escape] #'+escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command '+escape))


;;; General editing

(custom-theme-set-faces 'user
                        '(region ((((background light))
                                   (:foreground unspecified :background unspecified :inherit modus-themes-search-lazy))))
                        '(iedit-occurrence ((t (:inherit modus-themes-search-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Since I use evil, I have no need for the usual rectangular selection
;; keybinding.
(keymap-global-set "C-x SPC"
                   (defun +insert-char ()
                     "Insert a character at point."
                     (interactive)
                     (evil-insert-state)
                     (call-interactively
                      (if (equal system-type 'darwin)
                          #'ns-do-show-character-palette
                        #'insert-char))))

(when (equal system-type 'darwin)
  ;; Delete some unneeded macOS-like keybindings.
  (keymap-global-unset "s-o")
  (keymap-global-unset "s-f")
  (keymap-global-unset "s-q")
  (keymap-global-unset "s-t")
  (keymap-global-unset "s-n"))

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
(setq delete-pair-blink-delay 0.1)

(setq use-dialog-box nil)

(setq next-error-recenter '(4))
(setq find-library-include-other-files nil)

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

(keymap-global-set "C-c e e" #'toggle-debug-on-error)

(use-package window
  ;; Window management stuff that's not in the C layer.
  :general ("M-o" #'other-window)

  ;; Prefer vertical splits--better when the Emacs GUI window is wide rather
  ;; than tall.
  :custom
  (split-width-threshold 160)
  (split-height-threshold nil))

(use-package +window
  :general (:keymaps 'override-global-map
                     "M-f" #'+toggle-window-fullframe
                     "M-r" #'+toggle-side-window-raised))

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
  (backup-inhibited t)
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
  :config
  (define-advice after-find-file (:around (fn &rest args) dont-block-on-autosave-exists)
    "Prevent the editor blocking to inform you when an autosave file exists."
    (cl-letf (((symbol-function #'sit-for) #'ignore))
      (apply fn args))))

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
  (after-save-hook . +auto-revert-visible-buffers-h)
  (+switch-buffer-hook . +auto-revert-current-buffer-h)
  (+switch-window-hook . +auto-revert-current-buffer-h)
  :config
  (add-function :after after-focus-change-function #'+auto-revert-visible-buffers-h)

  :custom
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list ".")))

(use-package elec-pair
  ;; Automatically insert matching pairs.
  :after-call +first-file-hook +first-buffer-hook
  :init
  (electric-pair-mode +1))

(use-package lisp
  ;; Despite its name, provides many programming-language generic features.
  :general
  (:keymaps 'prog-mode-map :states 'normal "(" 'backward-sexp ")" 'forward-sexp))

(use-package puni :ensure t
  ;; Provides structured editing commands.
  :after-call +first-file-hook +first-buffer-hook
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general-config
  (:keymaps 'puni-mode-map :states 'insert "C-w" #'puni-backward-kill-word)
  (:keymaps 'puni-mode-map :states '(visual) "C-k" #'puni-kill-active-region)
  (:keymaps 'puni-mode-map :states '(insert normal emacs)
            "C-k" #'+kill-line
            "M-(" #'puni-wrap-round
            "M-[" #'puni-wrap-square
            "M-S-{" #'puni-wrap-curly))

(use-package recentf
  ;; Maintain a list of visited files.
  :after-call recentf consult-buffer
  :defer-incrementally t
  :custom
  (recentf-max-saved-items 100)
  :config
  (recentf-mode +1))

(keymap-global-set "C-c SPC"
                   (defun +insert-nbsp ()
                     (interactive)
                     (insert-char #x00A0)))

(use-package winner
  ;; Provides undo/redo for buffer & window layout changes.
  :general-config (:keymaps 'winner-mode-map
                            "M-<" #'winner-undo
                            "M->" #'winner-redo)
  :after-call +first-file-hook +first-buffer-hook
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

(use-package diff
  ;; Support for Unix diff files.
  :custom
  (diff-default-read-only t)
  (diff-advance-after-apply-hunk t)
  (diff-font-lock-prettify t)
  (diff-font-lock-syntax 'hunk-also))

(use-package ediff
  ;; Interactive file diff & merge UI.
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
  :config
  (use-package mod-compilation :demand t))

;; Disable bidirectional text by default.
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Don't render cursors or regions in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq redisplay-skip-fontification-on-input t)

(use-package elpaca
  ;; Configure aspects of elpaca not required for initial package bootstrap.
  :general-config
  (:states 'normal :keymaps 'elpaca-manager-mode-map "/" #'elpaca-ui-search)
  (:keymaps 'elpaca-info-mode-map "q" #'quit-window))

(use-package profiler
  :general-config
  (:keymaps 'profiler-report-mode-map
   :states 'normal
   "A" #'profiler-report-ascending-sort
   "D" #'profiler-report-descending-sort
   "K" #'profiler-report-describe-entry))

(use-package goto-addr
  ;; Turns URLs in the buffer into clickable buttons.
  :init
  (defun +goto-address-maybe-h ()
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (goto-address)
      (goto-address-mode +1)))
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook magit-process-mode-hook) . +goto-address-maybe-h))

(use-package better-jumper :ensure t
  ;; Maintains a jump list so you can more easily get back to where you were if
  ;; a command takes you somewhere else.
  :after-call +first-file-hook +first-buffer-hook
  :preface
  (defun +set-jump-point ()
    (when (get-buffer-window)
      (better-jumper-set-jump))
    nil)
  :init
  (better-jumper-mode +1)

  :config
  (add-hook 'kill-buffer-hook #'+set-jump-point)
  (advice-add #'consult-imenu :before #'+set-jump-point)
  (advice-add #'org-mark-ring-push :before #'+set-jump-point)
  (add-hook 'org-open-at-point-functions #'+set-jump-point)

  :general
  (:states 'normal
           "C-." #'better-jumper-jump-forward
           "C-," #'better-jumper-jump-backward)

  :general-config
  ([remap evil-jump-forward]  #'better-jumper-jump-forward
   [remap evil-jump-backward] #'better-jumper-jump-backward
   [remap xref-pop-marker-stack] #'better-jumper-jump-backward
   [remap xref-go-back] #'better-jumper-jump-backward
   [remap pop-tag-mark] #'better-jumper-jump-backward
   [remap xref-go-forward] #'better-jumper-jump-forward))

(use-package server
  ;; Use existing Emacs instances to edit files as $EDITOR.
  :if (display-graphic-p)
  :after-call +first-input-hook +first-file-hook
  :config
  (unless (server-running-p)
    (server-start)))

(use-package so-long
  ;; Improve performance of files with very long lines.
  :hook (elpaca-after-init-hook . global-so-long-mode))

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

(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
  (add-to-list 'trusted-content +lisp-dir)
  (add-to-list 'trusted-content +modules-dir))

;; Silence "For information about GNU Emacs and the GNU system..." on startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Don't tell me what key I could have used instead of M-x.
(advice-add #'execute-extended-command--describe-binding-msg :override #'ignore)

(use-package eshell
  ;; Emacs' built-in shell combining Emacs Lisp evaluation with Unix shell
  ;; features.
  :config
  (use-package mod-eshell :demand t))

(use-package eat :ensure t
  ;; A reasonably performant terminal emulator in Emacs
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :general
  (:keymaps 'project-prefix-map "s" #'eat-project)
  (:keymaps 'eat-semi-char-mode-map
            "s-v" 'eat-yank
            "M-o" nil
            [escape] 'eat-self-input)
  :init
  (with-eval-after-load '+evil-collection
    (pushnew! +evil-collection-disabled-list 'eat))

  :config
  ;; Disable evil-mode entirely in eat-mode buffers.
  (with-eval-after-load 'evil
    (pushnew! evil-buffer-regexps `(,(rx bol "*eat")))))

(use-package string-inflection :ensure t
  ;; Provides commands for cycling different string casing styles for the ident
  ;; at point, e.g. UpperCamelCase, lowerCamelCase, etc.
  :general (:states '(normal insert) "M--" #'string-inflection-all-cycle))

(use-package envrc :ensure t
  ;; Adds direnv support.
  :hook (+first-file-hook . envrc-global-mode)
  :custom
  (envrc-show-summary-in-minibuffer nil) ; very noisy.
  )

(use-package mise :ensure t
  ;; Adds support for mise - https://mise.jdx.dev.
  :hook (+first-file-hook . global-mise-mode)
  :if (executable-find "mise"))

(use-package exec-path-from-shell :ensure t
  ;; Use the shell to get some environment vars; necessary when the window
  ;; system runs Emacs under a very different process environment.
  ;;
  ;; Also, turns out we need this for direnv to work right in compilation buffers.
  :after-call +first-buffer-hook +first-file-hook
  :if (memq system-type '(darwin x))
  :demand t
  :config
  (pushnew! exec-path-from-shell-variables
            ;; Add variables needed for M-x compile with Nix. See:
            ;; https://github.com/purcell/envrc/issues/92#issuecomment-2415612472
            "SSH_AUTH_SOCK"
            "SSH_AGENT_PID"
            "XDG_DATA_DIRS"
            "XDG_CONFIG_DIRS"
            "__NIX_DARWIN_SET_ENVIRONMENT_DONE"
            "__HM_SESS_VARS_SOURCED"
            "NIX_USER_PROFILE_DIR"
            "NIX_SSL_CERT_FILE"
            "NIX_PROFILES"
            "NIX_PATH"
            ;; Extra environment variables set via home-manager.
            "RIPGREP_CONFIG_PATH"
            )

  ;; Speed up by using a non-interactive shell.
  (delq! "-i" exec-path-from-shell-arguments)

  (exec-path-from-shell-initialize))

(use-package ligature :ensure t
  ;; Teach Emacs how to display ligatures when available.
  :after-call +first-buffer-hook +first-file-hook
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode (+read-eld "ligatures/prog-mode.eld"))
  (ligature-set-ligatures 'compilation-mode (+read-eld "ligatures/prog-mode.eld"))
  (ligature-set-ligatures '(text-mode org-agenda-mode) (+read-eld "ligatures/text-mode.eld"))

  (global-ligature-mode t))

(use-package align
  ;; Emacs has an extensible mode-specific alignment system. In this age of code
  ;; formatters it's not terribly useful, but I do use `align-regexp' from
  ;; time-to-time.
  :general ("C-x a a" #'align-regexp))

(use-package rotate :ensure t
  ;; Provides a few commands for arranging windows in pre-configured
  ;; layouts--very handy.
  ;;
  ;; TODO: Define my own version that ignores side-windows when re-arranging.
  :commands (rotate-layout)
  :config
  (setq rotate-functions '(rotate:even-horizontal rotate:even-vertical)))

(use-package newcomment
  ;; Provides comment-related commands and variables to customise their
  ;; behaviour.
  :custom
  (comment-empty-lines t)
  (comment-multi-line t)
  (comment-style 'extra-line)
  :config
  (setq-default comment-column 0))

(use-package proced
  ;; User-process management UI.
  :custom
  (proced-enable-color-flag t))

(use-package replace
  ;; Defines search+replace functionality, including `occur'.
  :hook
  (occur-mode-hook . hl-line-mode))

(use-package grep
  ;; Buffers showing filesystem search results. The default program is grep;
  ;; change it to ripgrep.
  :custom
  (grep-use-headings t)
  (grep-template "rg --line-number --with-filename --null --regexp <R> <F>"))

(use-package wgrep :ensure t
  ;; Adds a mode for grep-like results buffers that allows you to edit the
  ;; underlying files directly.
  ;;
  ;; TODO: Replace with built-in `grep-edit-mode' once I'm on Emacs 31.
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

(use-package xref
  ;; Provides the interface for navigating symbol definitions & references, etc.
  :custom
  (xref-search-program 'ripgrep))

(use-package indent
  ;; Indentation behaviour.
  :custom
  (tab-first-completion 'word-or-paren-or-punct))

(use-package bufler :ensure t
  ;; A better buffer list than the default.
  :config
  (use-package mod-bufler :demand t))

(use-package mod-input-methods
  :init
  (keymap-global-set "M-i" 'activate-transient-input-method)
  :after-call toggle-input-method activate-transient-input-method)

(use-package tramp
  :config
  (pushnew! tramp-remote-path 'tramp-own-remote-path))


;;; Open some files as read-only, e.g. vendored deps.

(defun +file-should-be-opened-read-only-p (file)
  (let ((file (file-truename file)))
    (and
     ;; matches truthy
     (string-match-p (rx (or
                          ;;; These files should be read-only...

                          "/vendor/"
                          "/elpaca/"
                          "/node_modules/"

                          ))
                     file)
     (not
      ;; matches falsey
      (string-match-p (rx (or
                           ;;; ...except when they match these patterns.

                           "/.git/" ; Ensure we can still use git.
                           "/emacs/elpaca/repos/nursery/" ; My nursery repo is pulled in by elpaca
                           ))
                      file)))))

(add-hook! 'find-file-hook
  (when (+file-should-be-opened-read-only-p (buffer-file-name))
    (read-only-mode +1)))


;;; Visual enhancements

(use-package catppuccin-theme :ensure (:wait t) :demand t
  :init
  (setq +theme-dark 'catppuccin)
  (+theme-update))

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode-hook . hs-minor-mode))

(use-package page-break-lines :ensure t
  ;; Displays ^L page break characters as a horizontal rule. Useful for
  ;; demarcating sections of a file.
  :after-call +first-file-hook +first-buffer-hook
  :config
  (global-page-break-lines-mode +1)
  (pushnew! page-break-lines-modes 'rfc-mode 'prog-mode 'text-mode))

(use-package hide-mode-line :ensure (hide-mode-line
                                     :host github
                                     :repo "hlissner/emacs-hide-mode-line")
  ;; Disable the mode-line in situations where it's not useful.
  :hook ((completion-list-mode-hook
          Man-mode-hook
          ielm-mode-hook
          calendar-mode-hook
          eshell-mode-hook
          compilation-mode-hook
          help-mode-hook
          shell-command-mode-hook
          eat-mode-hook
          gptel-mode-hook
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

(use-package highlight-numbers :ensure (highlight-numbers
                                        :host github
                                        :repo "Fanael/highlight-numbers")
  ;; Ensure numbers always have syntax highlighting applied, even if a
  ;; major-mode neglects to configure that.
  :hook (prog-mode-hook conf-mode-hook)
  :custom (highlight-numbers-generic-regexp
           (rx symbol-start (+ digit) (? "." (* digit)) symbol-end)))

(use-package display-line-numbers
  ;; Show line-numbers in the margin.
  :init
  (setq-default display-line-numbers-width 3)
  (setq-default display-line-numbers-widen t))

(use-package hl-todo :ensure t
  ;; Display TODO comments with special highlights.
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

(use-package indent-bars :ensure t
  ;; Display indentation guides in buffers. Particularly useful for
  ;; indentation-sensitive language modes.
  :hook (yaml-ts-mode-hook python-ts-mode-hook)
  :custom
  (indent-bars-starting-column 0)
  (indent-bars-width-frac 0.15)
  (indent-bars-color-by-depth nil)
  (indent-bars-color '(font-lock-comment-face :face-bg nil :blend 0.425))
  (indent-bars-highlight-current-depth nil))

(use-package pulsar :ensure t
  ;; Temporarily highlights the current line after performing certain operations
  :hook (+first-input-hook . pulsar-global-mode)
  :custom
  (pulsar-iterations 5)
  (pulsar-pulse-on-window-change t)
  :config
  (use-package mod-pulsar :demand t))

(use-package hl-line
  ;; Highlight the current line.
  :custom
  (hl-line-sticky-flag nil))

(use-package whitespace
  ;; Visualise whitespace characters.
  :config
  (delq! 'newline whitespace-style)
  (delq! 'newline-mark whitespace-style))

(use-package paren
  ;; Provides `show-paren-mode', which highlights the matched pair at point.
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

(use-package paren-face :ensure t
  ;; Adds a face for parentheses in lisps. I hijack it to dim semicolons and
  ;; other non-critical syntax elements in other langs.
  :hook (lisp-data-mode-hook c-ts-base-mode-hook elixir-ts-mode-hook)

  :config
  (setq-hook! 'elixir-ts-mode-hook
    paren-face-regexp (rx (any "(),&")))

  (setq-hook! 'c-ts-base-mode-hook
    paren-face-regexp (rx (any ";,"))))

(use-package breadcrumb :ensure t
  :custom
  (breadcrumb-idle-time 0.3))


;;; Spell-checking

(use-package ispell
  ;; Built-in spellchecker. I don't actually use it directly, but other packages
  ;; reference its configuration.
  :custom
  (ispell-dictionary "en_AU")
  (ispell-personal-dictionary (file-name-concat org-directory "aspell.en.pws"))
  :config
  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))
  (ispell-set-spellchecker-params)

  ;; Treat aspell dictionaries as UTF-8. Note that aspell itself needs the
  ;; `utf-8' token in the header line for a dictionary file to use UTF-8
  ;; encoding.
  (add-to-list 'file-coding-system-alist
               (cons (rx "aspell." (+? nonl) ".pws" eos) 'utf-8-unix)))

(use-package spell-fu :ensure t
  ;; A more lightweight spell-checker than the built-in.
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general
  (:states '(normal motion)
           "zn" #'spell-fu-goto-next-error
           "zp" #'spell-fu-goto-previous-error
           "zg" #'spell-fu-word-add
           "zx" #'spell-fu-word-remove)

  :config
  (add-hook! 'spell-fu-mode-hook
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_AU"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "fr")))

  (setq-hook! 'org-mode-hook
    spell-fu-faces-exclude '(org-meta-line org-link org-code org-block
                             org-block-begin-line org-block-end-line
                             org-footnote org-tag org-modern-tag org-verbatim)))

(use-package flyspell-correct :ensure t
  ;; Provides a nicer command for working with spelling corrections.
  :after spell-fu
  :general
  (:states 'normal "z SPC" #'flyspell-correct-at-point))


;;; Dired

(use-package dired
  ;; Emacs' built-in file management interface.
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . hl-line-mode)
  :custom
  (dired-garbage-files-regexp (rx (or ".log" ".toc" ".dvi" ".bak" ".orig" ".rej" ".aux" ".DS_Store")
                                  eos))
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-kill-when-opening-new-dired-buffer t)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t)
  :config
  (+local-leader-set-key 'dired-mode-map
    "t" '(dired-toggle-marks :wk "toggle marks")
    "s" '(nil :wk "subdir")
    "si" '(dired-insert-subdir :wk "insert")
    "sx" '(dired-kill-subdir :wk "kill"))
  (setq-hook! 'dired-mode-hook
    dired-listing-switches (if (file-remote-p default-directory)
                               "-al"
                             "--almost-all --human-readable --group-directories-first --no-group")))

(use-package dired-x
  ;; Extra functionality around omitting files, etc.
  :hook (dired-mode-hook . dired-omit-mode)
  :custom
  (dired-omit-files (rx bos "."))
  :init
  (+local-leader-set-key 'dired-mode-map
    "h" '(dired-omit-mode :wk "toggle hidden files")))

(use-package nerd-icons :ensure t
  ;; Icon set used by various packages.
  :autoload nerd-icons-codicon nerd-icons-faicon)

(use-package nerd-icons-dired :ensure t
  ;; Show icons in dired.
  :hook dired-mode-hook)

(use-package wdired
  ;; Makes dired buffers directly editable; the changes are interpreted into
  ;; operations on the corresponding file names.
  :general
  (:keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(use-package diredfl :ensure t
  ;; Add extra font-lock to dired file listings.
  :hook (dired-mode-hook))


;;; evil-mode

(use-package evil :ensure t
  ;; Evil is a better vim emulation implementation than the one that
  ;; ships with Emacs.
  :demand t
  :general-config
  (:states 'emacs "ESC ESC" #'evil-normal-state)
  (:states '(insert normal emacs)
           "M-." #'xref-find-definitions
           "C-x RET" #'insert-char)
  :custom
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-v$-excludes-newline t)
  (evil-want-C-g-bindings)
  (evil-want-C-u-delete nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  :config
  (use-package mod-evil :demand t)
  (evil-mode +1)

  (add-hook '+escape-hook
            (defun +evil-disable-ex-highlights-h ()
              (when (evil-ex-hl-active-p 'evil-ex-search)
                (evil-ex-nohighlight)
                t)))

  :general-config
  (:keymaps +default-minibuffer-maps
            "C-a"    #'move-beginning-of-line
            "C-r"    #'evil-paste-from-register
            "C-u"    #'evil-delete-back-to-indentation
            "C-v"    #'yank
            "C-w"    (defun +delete-backward-word-no-kill (arg)
                       "Like `backward-kill-word', but doesn't affect the kill-ring."
                       (interactive "p")
                       (let ((kill-ring nil) (kill-ring-yank-pointer nil))
                         (ignore-errors (backward-kill-word arg))))))

(use-package vundo :ensure (vundo :host github :repo "casouri/vundo")
  ;; Visualise the Emacs undo history.
  :general ("C-x u" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package evil-collection :ensure t
  ;; Community-managed collection of evil keybindings; makes evil behave more
  ;; consistently across many modes.
  :custom
  ;; Ensure we do not overwrite the global leader key binding.
  (evil-collection-key-blacklist '("SPC" "S-SPC"))

  ;; Org-mode derives from outline-mode; disable the outline bindings to prevent
  ;; conflicts.
  (evil-collection-outline-enable-in-minor-mode-p nil)

  ;; Be a bit smarter about the evil-collection load sequence; in particular,
  ;; set up bindings in hooks first time we activate a major-mode. This makes
  ;; key binding setup more performant and more predictable.
  :init
  (with-eval-after-load 'evil
    (require '+evil-collection)
    (+evil-collection-defer-install-to-mode-activation))
  :config
  (+evil-collection-init 'comint)

  ;; Fix leader keybindings that get clobbered by evil-collection.

  (define-advice evil-collection-magit-init (:after (&rest _) bind-leader)
    (general-define-key :keymaps (append evil-collection-magit-maps
                                         evil-collection-magit-section-maps)
                        :states '(normal)
                        "SPC" #'+leader-key)))

(use-package evil-surround :ensure t
  ;; Evil-surround makes the S key work as an operator to surround an
  ;; object with, e.g., matched parentheses.
  :hook ((text-mode-hook prog-mode-hook) . evil-surround-mode)
  ;; Use lowercase 's' for surround instead of 'S'.
  :general-config (:states '(visual) :keymaps 'evil-surround-mode-map "s" #'evil-surround-region)
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\) . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\] . ("[" . "]"))
                               (?\{ . ("{" . "}"))
                               (?\} . ("{" . "}"))
                               (?# . ("#{" . "}"))
                               (?> . ("<" . ">"))
                               (?f . evil-surround-function)
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)))

  :config
  (add-hook! 'emacs-lisp-mode-hook
    (make-local-variable 'evil-surround-pairs-alist)
    (alist-set! evil-surround-pairs-alist ?` '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?' '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?f #'evil-surround-prefix-function)))

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
                 (equal last-command 'evil-visual-char) #'+multiedit))

  :general-config
  (:keymaps 'evil-multiedit-mode-map
   :states 'normal
   "Y" (defun +evil-multiedit-copy ()
         (interactive)
         (when-let* ((str (iedit-current-occurrence-string)))
           (kill-new str)
           (message "Copied to kill ring")))
   "<tab>" #'iedit-toggle-selection
   "n" #'evil-multiedit-next
   "N" #'evil-multiedit-prev
   "S" #'evil-multiedit--change-line))

(use-package expreg :ensure t
  ;; Use tree-sitter to mark syntactic elements.
  :init
  (defun +expreg-expand-n (n)
    "Expand to N syntactic units, defaulting to 1 if none is provided interactively."
    (interactive "p")
    (dotimes (_ n)
      (expreg-expand)))

  (defun +expreg-expand-dwim ()
    "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a
word.  Fall back to regular `expreg-expand'."
    (interactive)
    (when iedit-mode
      (iedit-done))
    (let ((symbol (bounds-of-thing-at-point 'symbol)))
      (cond
       ((equal (bounds-of-thing-at-point 'word) symbol)
        (+expreg-expand-n 1))
       (symbol (+expreg-expand-n 2))
       (t (expreg-expand))))))

(use-package evil-anzu :ensure t
  ;; Show an indication in the modeline of how many evil-search hits are in the
  ;; buffer, and which one point last moved to.
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))


;;; Navigation

(use-package avy :ensure t
  ;; Jump to things or execute other actions by typing a few letters.
  :general ("M-g" #'avy-goto-char-timer)

  :config
  (defun +avy-action-change-move (pt)
    "Delete the thing at PT and enter insert state."
    (goto-char pt)
    (avy-forward-item)
    (kill-region pt (point))
    (evil-insert-state)
    (point))

  (defun +avy-action-evil-lookup (pt)
    "Look up the definition of thing at PT with evil."
    (save-excursion
      (goto-char pt)
      (avy-forward-item)
      (evil-lookup))
    t)

  :custom
  ;; Customise the action keys to make actions a bit more vimmy.
  (avy-dispatch-alist '((?x . avy-action-kill-stay)
                        (?d . avy-action-kill-move)
                        (?c . +avy-action-change-move)
                        (?t . avy-action-teleport)
                        (?v . avy-action-mark)
                        (?y . avy-action-copy)
                        (?p . avy-action-yank)
                        (?P . avy-action-yank-line)
                        (?i . avy-action-ispell)
                        (?K . +avy-action-evil-lookup)
                        (? . avy-action-zap-to-char))))

;; Use +/- to mark syntactic elements with tree-sitter. However, if I don't have
;; a selection, make - call avy.
(general-define-key :states '(normal motion)
                    "-" (general-predicate-dispatch #'avy-goto-char-timer
                          (region-active-p) #'expreg-contract)
                    "+" #'+expreg-expand-dwim)

(use-package ace-window :ensure t
  ;; Jump to specific windows
  :general ("M-w" #'ace-window
            "M-o" #'other-window))


;;; Completion

(use-package vertico :ensure t
  ;; Vertico provides a better completion UI than the built-in default.
  :hook +first-input-hook
  :custom
  (vertico-preselect 'no-prompt)
  (vertico-cycle t)
  :general-config (:keymaps 'vertico-map
                            "C-<return>" #'vertico-exit-input
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
    :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

  (use-package vertico-repeat
    ;; Quickly restore the previous vertico command you ran.
    :hook (minibuffer-setup-hook . vertico-repeat-save)
    :config
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))

(use-package marginalia :ensure t
  ;; Marginalia shows extra information alongside minibuffer items
  ;; during completion.
  :hook +first-input-hook
  :general
  (:keymaps 'minibuffer-local-map "M-A" #'marginalia-cycle))

(use-package minibuffer
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
  (completion-category-defaults nil)

  :init
  (use-package orderless :ensure t
    ;; Orderless allows you to filter completion candidates by typing
    ;; space-separated terms in any order.
    :after-call +first-input-hook))

(use-package savehist
  ;; Persists Emacs completion history. Used by vertico.
  :init (savehist-mode +1)
  :custom
  (savehist-autosave-interval nil) ; on exit
  (history-delete-duplicates t)
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
    (let ((sans-brackets
           (replace-regexp-in-string (rx (or (and bos "[" (*? any) "]*")
                                             (and "[" (*? any) "]*" eos)))
                                     ""
                                     crm-separator)))
      (cons (format "[CRM %s] %s" (propertize sans-brackets 'face 'error) (car args))
            (cdr args)))))

(use-package corfu :ensure t
  ;; Corfu provides in-buffer completions as you type.
  :hook (+first-input-hook . global-corfu-mode)
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

(use-package nerd-icons-corfu :ensure t
  ;; Adds icons to corfu popups.
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape :ensure t
  ;; Adds useful functionality for `completion-at-point-functions'.
  :init
  (add-hook! 'prog-mode-hook
    (add-hook 'completion-at-point-functions #'cape-file -10 t))
  (add-hook! 'org-mode-hook
    (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))

  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

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
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3))

(use-package consult :ensure t
  ;; Consult provides commands for common tasks that leverage the Emacs
  ;; completion system. It composes well with the above packages.
  :general
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

(use-package embark :ensure t
  ;; Embark provides a UI for performing contextual actions on selected items
  ;; within completing-read.
  :general
  (:states '(normal emacs motion)
           "C-@" #'embark-act
           "C-t" #'embark-dwim)
  (:keymaps +default-minibuffer-maps
            "C-@" #'embark-act
            "C-c C-e" #'embark-export
            "C-c C-c" #'embark-collect))

(use-package embark-consult :ensure t
  ;; Integrate embark with consult
  :after (:any consult embark)
  :demand t
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(pushnew! completion-ignored-extensions
          ".DS_Store"
          ".eln"
          ".drv"
          ".direnv/"
          ".git/"
          )

(use-package minibuf-eldef
  ;; Set how the default option for empty input is displayed in the minibuffer.
  :hook (after-init . minibuffer-electric-default-mode)
  :custom
  (minibuffer-default-prompt-format " [%s]"))

(use-package dabbrev
  ;; Dynamically complete using identifier-like words entered in this or other
  ;; buffers.
  :custom
  (dabbrev-abbrev-skip-leading-regexp "[$*/=~']")
  (dabbrev-upcase-means-case-search t)
  :config
  (pushnew! dabbrev-ignored-buffer-modes
            'docview-mode 'pdf-view-mode))


;;; VC & magit

(use-package transient :ensure t
  ;; Lib for showing pop-up menus of key commands, often with switches to modify
  ;; behaviour.
  ;;
  ;; Magit depends on a more recent version of transient than the one that ships
  ;; with Emacs.
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))

(use-package magit :ensure t
  ;; Magit is the definitive UX for working with git.
  :config
  (use-package mod-magit :demand t)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-insert-related-refs nil)
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package git-timemachine :ensure t
  :general-config
  (:states 'normal
   :keymaps 'git-timemachine-mode-map
   "C-p" #'git-timemachine-show-previous-revision
   "C-n" #'git-timemachine-show-next-revision
   "gb"  #'git-timemachine-blame
   "gtc" #'git-timemachine-show-commit)

  :config
  ;; git-timemachine uses `delay-mode-hooks', which can suppress font-lock.
  (add-hook 'git-timemachine-mode-hook #'font-lock-mode)
  ;; Ensure evil keymaps are applied
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)

  ;; Show information in header-line for better visibility.
  :custom
  (git-timemachine-show-minibuffer-details t)
  :config
  (define-advice git-timemachine--show-minibuffer-details (:override (revision) use-header-line)
    "Show revision details in the header-line, instead of the minibuffer."
    (let* ((date-relative (nth 3 revision))
           (date-full (nth 4 revision))
           (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
           (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
      (setq header-line-format
            (format "%s%s [%s (%s)]"
                    (propertize author 'face 'git-timemachine-minibuffer-author-face)
                    (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                    date-full date-relative)))))

(use-package browse-at-remote :ensure t
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)

  :config
  (define-advice browse-at-remote--get-local-branch (:after-until () const-main)
    "Return 'main' in detached state."
    "main")

  ;; Integrate browse-at-remote with git-timemachine
  :config
  (define-advice browse-at-remote-get-url (:around (fn &rest args) git-timemachine-integration)
    "Allow `browse-at-remote' commands in git-timemachine buffers to open that
file in your browser at the visited revision."
    (if (bound-and-true-p git-timemachine-mode)
        (let* ((start-line (line-number-at-pos (min (region-beginning) (region-end))))
               (end-line (line-number-at-pos (max (region-beginning) (region-end))))
               (remote-ref (browse-at-remote--remote-ref buffer-file-name))
               (remote (car remote-ref))
               (ref (car git-timemachine-revision))
               (relname
                (file-relative-name
                 buffer-file-name (expand-file-name (vc-git-root buffer-file-name))))
               (target-repo (browse-at-remote--get-url-from-remote remote))
               (remote-type (browse-at-remote--get-remote-type target-repo))
               (repo-url (cdr target-repo))
               (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
          (unless url-formatter
            (error (format "Origin repo parsing failed: %s" repo-url)))
          (funcall url-formatter repo-url ref relname
                   (if start-line start-line)
                   (if (and end-line (not (equal start-line end-line))) end-line)))
      (apply fn args))))

(use-package forge :ensure t
  ;; Teach magit how to work with pull requests on GitHub and other git hosting
  ;; services.
  :after-call magit-status ; avoids compilation until first use
  :general
  (:keymaps 'magit-mode-map [remap magit-browse-thing] #'forge-browse)
  (:keymaps 'magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote)
  (:keymaps 'magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch)
  :config
  (use-package mod-forge :demand t))

(use-package vc
  :custom
  ;; Don't prompt when following links to files that are under version control.
  (vc-follow-symlinks t)
  ;; I literally only ever use Git these days.
  (vc-handled-backends '(Git))
  :config
  (pushnew! vc-directory-exclusion-list
            "node_modules"
            "cdk.out"
            "target"
            ".direnv"))

(use-package git-auto-commit-mode :ensure t
  ;; Provides a minor mode that automatically commits files as you edit--this is
  ;; useful mainly for my org files, where I don't care about commit messages
  ;; and just want a git history.
  :custom
  (gac-silent-message-p t)
  :init
  (put 'gac-debounce-interval 'safe-local-variable 'integerp)
  (put 'gac-automatically-add-new-files-p 'safe-local-variable 'booleanp))


;;; Modeline

(use-package doom-modeline :ensure t
  ;; The modeline from doom, packaged independently.
  :hook elpaca-after-init-hook
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-check-simple-format t)
  (doom-modeline-modal nil)
  :config
  (add-hook! 'magit-mode-hook
    (defun +modeline-hide-in-non-status-buffer-h ()
      "Show minimal modeline in magit-status buffer, no modeline elsewhere."
      (if (eq major-mode 'magit-status-mode)
          (doom-modeline-set-modeline 'magit)
        (hide-mode-line-mode)))))

(use-package anzu :ensure t
  ;; Display number of isearch results in the modeline
  :after-call isearch-mode)

(use-package evil-anzu
  ;; Extends anzu to evil-mode commands.
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))


;;; projects

(use-package project
  ;; Emacs' built-in project lib
  :custom
  (project-vc-ignores '(".cache/"))
  (project-switch-commands
   (defun +project-switch-magit-status ()
     (interactive)
     (let* ((proj (project-current t))
            (root (project-root proj)))
       (if (file-directory-p (file-name-concat root ".git"))
           (magit-status-setup-buffer root)
         (dired root)))))
  :config
  (project-remember-project (project-try-vc user-emacs-directory))
  (project-remember-project (project-try-vc org-directory)))


;;; Documentation systems

(use-package help
  ;; Defines the main help functionality for Emacs & Emacs-Lisp.
  :hook (help-mode-hook . turn-on-visual-line-mode)
  :custom
  (help-window-select t)
  :general
  (:keymaps 'help-map
            "h" nil ; view-hello-file: never intended, always annoying
            "l" #'find-library
            "c" #'describe-face
            "P" #'describe-text-properties))

(use-package help-mode
  ;; Major-mode for help buffers.
  :general-config
  (:keymaps 'help-mode-map :states 'normal
            "^" #'help-go-back
            "M-n" #'forward-button
            "M-p" #'backward-button
            "C-n" #'forward-button
            "C-p" #'backward-button))

(use-package helpful :ensure t
  ;; Extended help system, showing definitions etc. in the help buffer.
  :general (:keymaps 'help-map
                     "f" #'helpful-callable
                     "v" #'helpful-variable
                     "k" #'helpful-key))

(use-package eldoc
  ;; Display help hints in the echo area as you move around.
  :config
  ;; Teach eldoc to re-run after these commands.
  (eldoc-add-command 'evil-normal-state
                     'evil-insert
                     'evil-change
                     'evil-delete
                     'evil-replace))

(use-package info
  ;; Emacs' built-in system for reading texinfo manuals.
  :general
  (:keymaps 'help-map "s" #'info-apropos)
  :general-config
  (:keymaps 'Info-mode-map :states 'normal "^" #'Info-up
            "C-n" #'Info-forward-node
            "C-p" #'Info-backward-node))

(use-package man
  ;; Built-in manpage reader.
  :custom
  ;; Tell man to use pop-to-buffer under the hood, which uses display-buffer and
  ;; selects the window.
  (Man-notify-method 'aggressive))

(use-package imenu
  ;; Emacs' built-in navigator for points of interest in a buffer.
  :general-config
  (:keymaps 'Info-mode-map [remap consult-imenu] #'Info-menu))

(use-package rfc-mode :ensure t
  ;; Interface for browsing RFC documents.
  :general (:keymaps 'help-map "w" #'rfc-mode-browse)
  :custom
  (rfc-mode-directory "~/.cache/emacs/rfc-mode/rfcs/")
  :config
  ;; Fix the default face, which doesn't allow highlights (e.g. in vertico).
  (custom-theme-set-faces 'user
                          '(rfc-mode-browser-title-face ((t (:inherit bold)))))
  (with-eval-after-load 'evil
    (evil-set-initial-state 'rfc-mode 'motion)))


;;; Text & programming modes

(use-package text-mode
  ;; Emacs' general parent mode for non-programming-language text files.

  :mode  ("/LICENSE\\'")

  ;; Not sure of the performance impact of this... leave off for now.
  ;;
  ;; :hook (text-mode-hook . visual-line-mode)

  :custom
  (text-mode-ispell-word-completion nil))

(use-package lisp-mode
  ;; General configuration for all derived lisp modes.
  :config
  (add-hook! '(lisp-data-mode-hook emacs-lisp-mode-hook)
    (add-hook 'before-save-hook #'check-parens nil t)))

(use-package elisp-mode
  :config
  (use-package mod-emacs-lisp :demand t))

(use-package ert
  :general
  (:keymaps '(ert-results-mode-map emacs-lisp-mode-map)
            "C-c C-t" #'+ert)
  (:states 'motion :keymaps 'ert-results-mode-map
           "L" 'ert-results-toggle-printer-limits-for-test-at-point
           "T" 'ert-results-pop-to-timings
           "B" 'ert-results-pop-to-backtrace-for-test-at-point
           "H" 'ert-results-describe-test-at-point
           "M-n" 'ert-results-next-test
           "M-p" 'ert-results-previous-test)
  :init
  (defun +ert (arg)
    (interactive "p")
    (if arg
        (ert t)
      (call-interactively #'ert))))

(use-package nix-ts-mode :ensure t
  :mode "\\.nix\\'"
  :hook (nix-ts-mode-hook . eglot-ensure)
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-extra-root-markers "flake.nix"))
  (add-to-list 'auto-mode-alist `(,(rx "/flake.lock" eos) . json-ts-mode))
  :config
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt")))

  (setq-hook! 'nix-ts-mode-hook apheleia-formatter 'nixpkgs-fmt)

  (with-eval-after-load 'project
    (pushnew! project-vc-extra-root-markers "flake.nix")))

(use-package json-ts-mode
  :hook (json-ts-mode-hook . eglot-ensure))

(use-package yaml-ts-mode
  :hook (yaml-ts-mode-hook . eglot-ensure)
  :config
  (setq-hook! 'yaml-ts-mode-hook
    tab-width 2))

(use-package js
  :mode ("\\.[mc]?js" . js-ts-mode))

(use-package typescript-ts-mode
  :hook (typescript-ts-mode-hook . eglot-ensure)
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-ignores ".nx/")
    (pushnew! project-vc-extra-root-markers "nx.json" "cdk.json"))
  :config
  (pushnew! find-sibling-rules
            ;; Tests -> impl
            (list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
                  (rx (backref 1) ".ts"))
            ;; Impl -> tests
            (list (rx (group (+? any)) ".ts" eos)
                  (rx (backref 1) ".test.ts")
                  (rx (backref 1) ".integration.ts"))))

(use-package c-ts-mode
  :general-config
  (:keymaps 'c-ts-mode-map :states 'insert
            "<" #'+c-electric-left-angle-bracket)

  (:keymaps 'c-ts-mode-map :states '(normal insert)
            "S-<return>" #'+c-auto-insert-semi-newline)
  :init
  (defun +c-auto-insert-semi-newline ()
    (interactive)
    (goto-char (line-end-position))
    (unless (thing-at-point-looking-at (rx (any "{:;") (* space) eol))
      (insert ";"))
    (evil-insert-state)
    (newline-and-indent))

  (defun +c-electric-left-angle-bracket (&optional arg)
    (interactive "P")
    (let* ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
           (include-line-p (string-match-p (rx bol (* space) "#" (* space) "include" symbol-end)
                                           current-line)))
      (cond (include-line-p
             (just-one-space)
             (insert "<")
             (save-excursion
               (insert ">")))
            (t
             (call-interactively #'self-insert-command))))))

(use-package zig-mode :ensure t
  :mode "\\.\\(zig\\|zon\\)\\'"
  :custom
  (zig-format-on-save nil) ; use apheleia instead.
  )

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
  :mode ("rc\\'" "\\.dockerignore\\'" "\\.gitignore\\'"))

(use-package treesit-auto :ensure t
  ;; Automatic installation of treesitter grammars.
  :after-call +first-buffer-hook +first-file-hook
  :commands global-treesit-auto-mode
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

(use-package flymake
  ;; Frontend for in-buffer error checking & navigation.
  ;;
  ;; c.f. `next-error' and friends, which operate on compilation & grep results
  ;; across any number of buffers.
  :hook (prog-mode-hook . flymake-mode)
  :general-config (:keymaps 'flymake-mode-map
                            "M-n" #'flymake-goto-next-error
                            "M-p" #'flymake-goto-prev-error))

(use-package eglot
  ;; Emacs' built-in LSP integration.
  :general-config
  (:keymaps 'eglot-mode-map
   :states '(insert normal)
   "M-RET" #'eglot-code-actions)
  (:keymaps 'eglot-mode-map
   :states '(normal)
   "C-c C-r" #'eglot-rename)

  ;; Make RET open markdown links in the eldoc buffer.
  ;;
  ;; https://github.com/joaotavora/eglot/discussions/1238
  :config
  (defun +eglot-open-link ()
    (interactive)
    (if-let* ((url (get-text-property (point) 'help-echo)))
        (browse-url url)
      (user-error "No URL at point")))

  (define-advice eldoc-display-in-buffer (:after (&rest _) update-keymap)
    (with-current-buffer eldoc--doc-buffer
      (general-define-key :keymaps 'local :states 'motion "RET" #'+eglot-open-link)))

  ;; Prevent display of inlay hints
  (add-hook 'eglot-managed-mode-hook
            (defun +eglot-inlay-hints-off ()
              (eglot-inlay-hints-mode -1))))

(use-package eglot-booster :ensure (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  ;; Teach eglot to use lsp-booster for better performance.
  :after eglot
  :demand t
  :config (eglot-booster-mode +1))

(use-package markdown
  :general-config (:keymaps 'markdown-mode-map "C-c f" #'markdown-insert-footnote)
  :hook (markdown-mode-hook . visual-line-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  :config
  (+local-leader-set-key 'markdown-mode-map
    "l" '(markdown-toggle-url-hiding :wk "toggle URLs")
    "f" '(markdown-insert-footnote :wk "insert footnote")))

(use-package hcl-mode :ensure t
  :mode ("\\.hcl\\'")
  :config
  (+define-file-template (rx "terragrunt.hcl" eos) "terragrunt/terragrunt.eld")
  (+define-file-template (rx "root.hcl" eos) "terragrunt/root.eld")
  (+define-file-template (rx "region.hcl" eos) "terragrunt/region.eld")

  (with-eval-after-load 'apheleia-formatters
    (add-to-list 'apheleia-formatters '(terragrunt . ("terragrunt" "hcl" "fmt" "--stdin")))
    (alist-set! apheleia-mode-alist 'hcl-mode 'terragrunt)))

(use-package terraform-mode :ensure t
  :mode ("\\.tf\\'")
  :config
  ;; Use `tofu' for formatting terraform files if on PATH.
  (with-eval-after-load 'apheleia-formatters
    (add-to-list 'apheleia-formatters '(opentofu . ("tofu" "fmt" "-")))
    (alist-set! apheleia-mode-alist 'terraform-mode 'opentofu)))

(use-package elixir-ts-mode
  :mode ("\\.ex\\'" "\\.exs\\'")
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-extra-root-markers "mix.exs"))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls")))

  (pushnew! find-sibling-rules
            ;; Impl -> tests
            (list (rx (group-n 1 (+? nonl)) "/lib/" (group-n 2 (+? any)) ".ex" eos)
                  (rx (backref 1) "/test/" (backref 2) "_test.exs"))

            ;; Tests -> impl
            (list (rx (group-n 1 (+? nonl)) "/test/" (group-n 2 (+? any)) "_test.exs" eos)
                  (rx (backref 1) "/lib/" (backref 2) ".ex"))))

(use-package inf-elixir :ensure t)

(use-package erlang
  :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode))
  :init
  (with-eval-after-load 'dired
    (pushnew! completion-ignored-extensions ".jam" ".vee" ".beam"))
  (with-eval-after-load 'dired-x
    (pushnew! dired-omit-extensions ".jam" ".vee" ".beam")))

(use-package rust-ts-mode
  :hook (rust-ts-mode-hook . eglot-ensure)
  :config
  (setq-hook! 'rust-ts-mode-hook separedit-default-mode 'markdown-mode)
  ;; Make cargo commands run from Emacs look pretty.
  (setenv "CARGO_TERM_COLOR" "always"))

(use-package sh-script
  :init
  (add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))
  :config
  (define-advice sh-set-shell (:around (fn &rest args) silence-messages)
    (cl-letf (((symbol-function 'message) #'ignore))
      (apply fn args))))

;; Use tree-sitter modes.

(alist-set! major-mode-remap-alist #'c-mode #'c-ts-mode)

;; Make shell-scripts etc executable on save.

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)


;;; Debuggers

(use-package debug
  ;; The built-in debugger for the Emacs Lisp runtime.
  :config
  (use-package mod-debug :demand t))


;;; Code formatting

(use-package apheleia :ensure t
  ;; Apply code formatting on save. Works for a range of languages.
  :after-call +first-file-hook
  :custom
  (apheleia-remote-algorithm 'local)
  :config
  (apheleia-global-mode +1))

;; By default, trim trailing whitespace aggressively.

(defvar-local +trim-trailing-whitespace-aggressively t)

(add-hook! 'before-save-hook
  (when +trim-trailing-whitespace-aggressively
    (delete-trailing-whitespace)))

;; TODO: Evaluate whether ws-butler is something I need. Do I ever work in
;; codebases where I want to preserve existing trailing whitespace?

(use-package ws-butler :ensure t :disabled t
  ;; Delete trailing whitespace on visited lines.
  :hook (prog-mode-hook text-mode-hook conf-mode-hook)
  :config
  (pushnew! ws-butler-global-exempt-modes
            'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode))


;;; Templating

(use-package tempel :ensure t
  ;; Text snippets.
  ;;
  ;; NB. Field navigation uses M-{ and M-}.
  (:keymaps 'tempel-map :states 'normal "<escape>" #'tempel-done)
  :custom
  (tempel-path (file-name-concat +templates-dir "*.eld"))
  :init
  (add-hook! '(prog-mode-hook text-mode-hook config-mode-hook)
    (add-hook 'completion-at-point-functions #'tempel-expand -90 t)))

(use-package autoinsert
  :after-call +first-buffer-hook +first-file-hook
  :custom
  (auto-insert-directory (file-name-concat user-emacs-directory "file-templates/"))
  (auto-insert-alist nil)
  (auto-insert-query nil)
  :config
  (auto-insert-mode +1))

(use-package +file-templates
  :after autoinsert
  :demand t
  :config
  (require 'string-inflection nil t)

  (+define-file-template (rx ".el" eos) "emacs-lisp.eld")
  (+define-file-template (rx "flake.nix" eos) "flake.eld")

  (+define-file-template-dispatcher 'elixir-ts-mode
    ((string-match-p "/lib/" (buffer-file-name))
     "elixir/lib.eld")
    ((string-match-p (rx "/test/" (+? nonl) ".exs" eos) (buffer-file-name))
     "elixir/test.eld"))

  (defun +cdk-project-p (&optional dir)
    (locate-dominating-file (or dir default-directory) "cdk.json"))

  (defun +index-ts-p (file)
    (equal "index.ts" (file-name-nondirectory (buffer-file-name))))

  (+define-file-template-dispatcher 'typescript-ts-mode
    ((and (string-match-p "construct" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/construct.eld")
    ((and (string-match-p "/stacks/" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/stack.eld")))


;;; org-mode

;; org is a chonker; decompose the load process into smaller, incrementally
;; loaded features so it's less noticeable.
(+load-packages-incrementally '(calendar find-func format-spec org-macs org-compat org-faces org-entities
                                org-list org-pcomplete org-src org-footnote org-macro ob org org-modern
                                org-habit org-agenda org-capture))

(use-package org :ensure t ; NB. installed from org package archive.
  ;; org-mode - the reason why I can probably never switch to another editor.

  :hook ((org-mode-hook . abbrev-mode)
         (org-mode-hook . auto-fill-mode))

  :general ("C-c a" #'org-agenda)

  :custom
  (abbrev-file-name (file-name-concat org-directory "abbrev.el"))
  (org-babel-load-languages '((emacs-lisp . t)
                              (C . t)
                              (calc . t)
                              (shell . t)))
  :config
  (use-package mod-org :demand t)
  (use-package mod-org-link :after ol :demand t)
  (use-package mod-org-capture :after org-capture :demand t)
  (use-package mod-org-agenda :after org-agenda :demand t))

(use-package evil-org :ensure t
  ;; Provides extra evil keybindings for org-mode, org-agenda etc.
  :hook (org-mode-hook . evil-org-mode)
  :custom
  (evil-org-key-theme '(todo navigation insert textobjects additional calendar))
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :demand t
    :config
    (evil-org-agenda-set-keys)
    (evil-define-key 'motion org-agenda-mode-map
      (kbd "v") #'org-agenda-view-mode-dispatch
      (kbd "SPC") nil
      (kbd "/") #'org-agenda-filter)))

(use-package org-super-agenda :ensure t
  ;; Group items in the agenda
  :after org-agenda
  :demand t
  :custom
  (org-super-agenda-hide-empty-groups t)
  :config
  (org-super-agenda-mode +1)
  ;; Clear the keymap to ensure the regular evil keybindings for org-agenda
  ;; work.
  (setq org-super-agenda-header-map (make-sparse-keymap)))

(use-package org-roam :ensure t
  ;; Provides workflows for working with documents for atomic notes (e.g. a
  ;; Zettelkasten); implements backlinks between documents for discovering
  ;; connected notes.
  :after org
  :config
  (use-package mod-org-roam :demand t)

  :defer-incrementally
  ansi-color dash f rx seq magit-section emacsql

  ;; Autoload entrypoints
  :commands (org-roam-buffer-toggle)
  :init
  (+local-leader-set-key 'org-mode-map "<tab>" #'org-roam-buffer-toggle)
  :general
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'org-roam-node-insert))

(use-package separedit :ensure t
  ;; Easily pop open comments or strings for editing in a dedicated buffer.
  )

(use-package org-modern :ensure t
  ;; Provides visual enhancements that make org-mode look less cluttered and
  ;; more in-line with modern UX ideas.
  :after org
  :demand t
  :custom
  (org-modern-hide-stars nil)
  (org-modern-fold-stars
   '(("▶" . "▼") ("▹" . "▿") ("▸" . "▾") ("⯈" . "⯆")))
  (org-modern-block-name
   `(("src" . ("" "◌"))
     ("quote" . ("" "◌"))
     ("example" . ("" "◌"))))
  :config
  (global-org-modern-mode +1)
  (custom-theme-set-faces 'user
                          '(org-todo ((t (:bold t :inverse-video t))))
                          ;; Based on `eldoc-highlight-function-argument'
                          '(org-modern-date-active ((((background dark))
                                                     (:foreground "#dfaf7a"
                                                      :background "#381d0f"
                                                      :weight light
                                                      :inherit org-modern-label))
                                                    (((background light))
                                                     (:foreground "#884900"
                                                      :background "#f8f0d0"
                                                      :weight light
                                                      :inherit org-modern-label)))))

  (let ((custom-todos
         '(("WAIT" warning org-todo)
           ("PROJECT" font-lock-keyword-face org-todo))))

    (setq org-modern-todo-faces custom-todos)
    (setq org-todo-keyword-faces
          (seq-map (apply-partially #'take 2) custom-todos))))

(use-package org-cliplink :ensure t
  ;; Create org-mode links from URLs on the clipboard.
  :general (:keymaps 'org-mode-map "C-c l" #'org-cliplink))

(use-package calendar
  ;; The calendar widget used in org-mode.
  :custom
  (calendar-mode-line-format nil)
  (calendar-date-style 'iso)
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1)
  ;; :config
  ;; (setf (car calendar-time-display-form) '24-hours)
  )

(use-package ox-gfm :ensure t
  ;; Exporter backend for github-flavoured markdown.
  :after ox
  :demand t)


;;; Wide World of Web

(use-package elfeed :ensure t)



(use-package gptel :ensure t
  ;; Provides LLM integrations.
  :custom
  (gptel-model 'claude-sonnet-4-20250514)
  :init
  (use-package mod-gptel
    :general (:states 'visual "RET" #'+gptel-rewrite-fast))
  :config
  (use-package mod-gptel :demand t))

(use-package claude-code-ide :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  ;; Creates an MCP bridge between Emacs and Claude-Code.
  :after-call +first-input-hook +first-file-hook
  :general ()
  :custom
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package nursery :ensure (nursery :host github
                                      :repo "chrisbarrett/nursery"
                                      :files (:defaults "extensions/*"))
  :init
  (use-package org-roam-review
    :commands (org-roam-review org-roam-review-list-recently-added)
    :hook (org-roam-review-mode . toggle-truncate-lines)
    :general-config
    (:states '(normal) :keymaps 'org-roam-review-mode-map
             ;; "TAB" 'magit-section-cycle
             "/"   #'org-roam-review-modify-tags
             "g r" #'org-roam-review-refresh))

  (use-package org-roam-search
    :commands (org-roam-search))

  (use-package org-roam-links
    :commands (org-roam-links))

  (use-package org-roam-refill-previews
    :after org-roam
    :demand t
    :config
    (add-hook 'org-roam-preview-postprocess-functions #'org-roam-refill-previews))

  (use-package org-roam-lazy-previews
    :after org-roam
    :demand t)

  (use-package timekeep
    :commands (timekeep-start timekeep-stop)
    :general
    ("<f12>" (general-predicate-dispatch 'timekeep-start
               (and (fboundp 'org-clocking-p) (org-clocking-p)) 'timekeep-stop)))

  (use-package org-roam-rewrite
    :init
    (+local-leader-set-key 'org-mode-map
      "r" '(nil :wk "node refactoring")
      "rr" #'org-roam-rewrite-rename
      "ri" #'org-roam-rewrite-inline
      "re" #'org-roam-rewrite-extract
      "rD" #'org-roam-rewrite-remove))

  (use-package org-roam-dblocks
    :hook (org-mode-hook . org-roam-dblocks-autoupdate-mode))

  (use-package org-roam-slipbox
    :after org-roam
    :demand t
    :config
    (org-roam-slipbox-tag-mode +1)
    (+local-leader-set-key 'org-mode-map
      "rR" #'org-roam-slipbox-refile)))


;;; Load site files

(when (file-directory-p +site-files-directory)
  (dolist (file (directory-files +site-files-directory t (rx ".el" eos)))
    (unless (file-directory-p file)
      (load file t nil nil t))))



(add-hook! '+first-input-hook
  (use-package mod-display-buffer :demand t))

;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
