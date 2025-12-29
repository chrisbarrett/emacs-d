;;; init.el --- main Emacs init file -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; Suppress warning when loading Elpaca with latest Emacs.
(add-to-list 'warning-suppress-types '(elpaca core \30.2))
(add-to-list 'warning-suppress-types '(elpaca core \31.0.50))

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

(use-package auto-compile :ensure t :demand t
  ;; Ensure we never attempt to load outdated ELC files.
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Load init/**.el

(dolist (file (directory-files-recursively +init-dir (rx ".el" eos)))
  (load-file file))


;; Adapt the escape key customisation from Doom.

(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map))

(general-def :keymaps +default-minibuffer-maps "s-v" #'yank)

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
(general-def :states 'normal [escape] #'+escape)

(general-def :keymaps +default-minibuffer-maps [escape] #'+escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command '+escape))


;;; General editing

(custom-theme-set-faces 'user
                        '(region ((((background light))
                                   (:inherit lazy-highlight))))
                        '(iedit-occurrence ((t (:inherit query-replace))))
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
  :general (:keymaps 'override-global-map "M-o" #'other-window)

  ;; Prefer vertical splits--better when the Emacs GUI window is wide rather
  ;; than tall.
  :custom
  (split-width-threshold 160)
  (split-height-threshold nil))

;; Tiling window navigation--I configure bindings here that only work in GUI
;; frames. In terminals I use Zellij.
;;
;; NB terminals can't interpret M-S-* or M-C-* bindings, making these bindings
;; GUI-frame-only.

(use-package windmove
  :general (:keymaps 'override-global-map
                     "M-C" #'windmove-up
                     "M-H" #'windmove-left
                     "M-N" #'windmove-right
                     "M-T" #'windmove-down))

(use-package mod-tty-frames :demand t)

(use-package +window
  :general (:keymaps 'override-global-map
                     "M-f" #'+toggle-window-fullframe
                     "M-r" #'+toggle-side-window-raised
                     "M-S-<return>" #'+toggle-window-fullframe
                     "C-M-c" #'+win-swap-up
                     "C-M-h" #'+win-swap-left
                     "C-M-n" #'+win-swap-right
                     "C-M-t" #'+win-swap-down))

(use-package frame
  ;; Frame management settings
  :custom
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  :init
  (window-divider-mode +1))

(use-package tab-bar
  ;; Emacs' built-in tab-bar. I use it pretty much just use it for git
  ;; worktrees.
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
    (unless (or auto-revert-mode
                (active-minibuffer-window)
                ;; Skip non-file buffers
                (not (buffer-file-name))
                ;; Skip temporary/internal buffers
                (string-prefix-p " " (buffer-name)))
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
            "M-(" #'puni-wrap-round "M-)" #'puni-wrap-round
            "M-]" #'puni-wrap-square ;; NB. M-[ translates to the ESC-[ control sequence in terminals.
            "M-{" (general-predicate-dispatch #'puni-wrap-curly
                    ((and (fboundp 'tempel--active-p)
                          (tempel--active-p nil (current-buffer)))
                     #'tempel-previous))
            "M-}" (general-predicate-dispatch #'puni-wrap-curly
                    ((and (fboundp 'tempel--active-p)
                          (tempel--active-p nil (current-buffer)))
                     #'tempel-next))))

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
                            "M-<left>" #'winner-undo
                            "M-<right>" #'winner-redo)
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
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

(use-package clipetty :ensure t
  ;; Copy to and paste from GUI clipboard in terminal.
  :after-call +first-input-hook +first-file-hook
  :config (global-clipetty-mode +1))

(use-package xt-mouse
  :demand t
  :config
  (xterm-mouse-mode +1))

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
     (if (when-let* ((keys (this-single-command-raw-keys)))
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

(defconst +chrisbarrett-elpaca-repos (seq-map (lambda (repo)
                                                (file-name-concat elpaca-repos-directory (concat repo "/")))
                                              '("emacs-beads" "nursery"))
  "List of my repos managed via elpaca.")

(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
  (add-to-list 'trusted-content +init-dir)
  (add-to-list 'trusted-content +lisp-dir)
  (add-to-list 'trusted-content +modules-dir)
  (dolist (repo +chrisbarrett-elpaca-repos)
    (add-to-list 'trusted-content (abbreviate-file-name repo))))

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
  (:keymaps 'eat-semi-char-mode-map
            "s-v" 'eat-yank
            ;; Commands that should be intercepted rather than be passed to eat
            "M-B" nil
            "M-P" nil
            "M-m" nil
            "M-o" nil
            "M-<" nil
            "M->" nil
            ;; Commands that should be passed through
            "C-u" 'eat-self-input
            "C-o" 'eat-self-input
            [escape] 'eat-self-input)
  :init
  (with-eval-after-load '+evil-collection
    (pushnew! +evil-collection-disabled-list 'eat))

  :config
  ;; Disable evil-mode entirely in eat-mode buffers.
  (with-eval-after-load 'evil
    (pushnew! evil-buffer-regexps `(,(rx bol "*eat"))))

  ;; Prevent over-scrolling beyond buffer content in eat buffers
  (add-hook 'eat-mode-hook
            (defun +eat-prevent-overscroll-h ()
              "Prevent scrolling beyond buffer content in eat buffers."
              (setq-local scroll-conservatively 101)
              (setq-local maximum-scroll-margin 0.5))))

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
            "XDG_CACHE_HOME"
            "XDG_CONFIG_DIRS"
            "XDG_DATA_DIRS"
            "XDG_STATE_HOME"
            "__NIX_DARWIN_SET_ENVIRONMENT_DONE"
            "__HM_SESS_VARS_SOURCED"
            "NIX_USER_PROFILE_DIR"
            "NIX_SSL_CERT_FILE"
            "NIX_PROFILES"
            "NIX_PATH"
            ;; Extra environment variables set via home-manager.
            "RIPGREP_CONFIG_PATH"
            "NIX_EMACS_DARWIN_PATH_EXTRAS"
            )

  ;; Speed up by using a non-interactive shell.
  (delq! "-i" exec-path-from-shell-arguments)

  (exec-path-from-shell-initialize)
  (when-let* ((path-from-nix (getenv "NIX_EMACS_DARWIN_PATH_EXTRAS"))
              (paths (string-split path-from-nix ":")))
    (eval `(pushnew! exec-path ,@paths))
    (setenv "PATH" (string-join exec-path ":"))))

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

(use-package midnight
  ;; Run a hook at midnight; cleans up old buffers by default. Useful for
  ;; preventing an Emacs server instance from drowning in open buffers
  ;; throughout the work week.
  ;;
  ;; The behaviour of the buffer cleanup can be customised via the
  ;; `clean-buffer-list-*' variables.
  :after-call +first-file-hook +first-buffer-hook
  :config
  (midnight-mode +1))


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
      (string-match-p (rx-to-string `(or
                           ;;; ...except when they match these patterns.
                                      "/.git/" ; Ensure we can still use git.
                                      ,@+chrisbarrett-elpaca-repos))
                      file)))))

(add-hook! 'find-file-hook
  (when (+file-should-be-opened-read-only-p (buffer-file-name))
    (read-only-mode +1)))


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


;;; projects

(use-package project
  ;; Emacs' built-in project lib
  :custom
  (project-vc-ignores '(".cache/"))
  (project-list-exclude (list
                         (rx bol "/nix/store/")
                         (defun +project-exclude-hidden-dirs (project)
                           "Exclude projects in any hidden directory, except for the ~/.config dir."
                           (let ((root (project-root project)))
                             (and (string-match-p  (rx "/.") root)
                                  (not (string-prefix-p "~/.config/" root)))))))
  :config
  (use-package mod-project
    :demand t
    :general
    (:keymaps 'project-prefix-map "R" #'+projects-rescan))

  (project-remember-project (project-try-vc user-emacs-directory))
  (project-remember-project (project-try-vc org-directory)))

(use-package beframe :ensure t
  ;; Associate frames with lists of buffers; useful for workflows where you have
  ;; separate frames per project.
  :demand t
  :init
  (use-package mod-beframe
    :general (:keymaps 'override-global-map
                       "s-t" 'project-switch-beframed
                       "M-W" 'project-switch-beframed)
    :general (:keymaps 'project-prefix-map
                       "p" #'project-switch-beframed)
    :demand t
    :after beframe))


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

(use-package flymake-eldev :ensure t
  ;; Teach flymake to support eldev projects.
  ;;
  ;; See: https://emacs-eldev.github.io/eldev
  ;;
  ;; NOTE: Initialised automatically via an autoloaded form.
  )

(use-package buttercup :ensure t
  ;; A BDD-style testing framework for Elisp.
  ;;
  ;; Ordinarily it will be installed in relevant packages by eldev; however,
  ;; it's nice to have here so I can load it and get macro indentation right.
  :demand t
  :after elisp-mode)

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
  ;; Activate on shebangs for node, deno, bun etc.
  (add-to-list 'magic-mode-alist (cons (rx bol "#!" (+? nonl) (or "/" " ") (or "deno" "node" "bun" "tsx")
                                           (* space) (or eol (and "--" (+ nonl))))
                                       'typescript-ts-mode))
  :config
  (use-package mod-typescript :demand t))

(use-package c-ts-mode
  :general-config
  (:keymaps 'c-ts-mode-map :states 'insert
            "<" #'+c-electric-left-angle-bracket)

  (:keymaps 'c-ts-mode-map :states '(normal insert)
            "S-RET" #'+c-auto-insert-semi-newline)
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

  ;; Cache the expensive grammar detection to avoid TTY performance issues
  (defvar +treesit-auto--cached-remap-alist nil
    "Cache for treesit-auto major mode remap alist.")

  (define-advice treesit-auto--build-major-mode-remap-alist
      (:around (orig-fn &rest args) cache-remap-alist)
    "Cache the result of building the major mode remap alist to avoid expensive
subprocess calls on every file open, especially problematic in TTY."
    (or +treesit-auto--cached-remap-alist
        (setq +treesit-auto--cached-remap-alist
              (apply orig-fn args))))

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
  :custom
  (eglot-code-action-indicator "")

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

(use-package markdown-mode
  :general-config
  (:keymaps 'markdown-mode-map "C-c f" #'markdown-insert-footnote)
  (:keymaps 'markdown-mode-map :states 'normal "SPC n s" #'markdown-narrow-to-subtree)

  :hook ((markdown-mode-hook . visual-line-mode)
         (markdown-ts-mode-hook . visual-line-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  :commands (gfm-mode)
  :init
  (defun +markdown-ts-mode-maybe-gfm ()
    "Use gfm-mode in git repos, markdown-ts-mode otherwise."
    (if (and buffer-file-name (vc-git-root buffer-file-name))
        (gfm-mode)
      (markdown-ts-mode)))
  (add-to-list 'major-mode-remap-alist '(markdown-ts-mode . +markdown-ts-mode-maybe-gfm))
  :config
  (use-package mod-markdown :demand t))

(use-package hcl-mode :ensure t
  :mode ("\\.hcl\\'")
  :init
  (with-eval-after-load 'project
    (pushnew! project-vc-ignores ".drift-history.json"))
  :config
  (with-eval-after-load 'apheleia-formatters
    (add-to-list 'apheleia-formatters '(terragrunt . ("terragrunt" "hcl" "fmt" "--stdin")))
    (alist-set! apheleia-mode-alist 'hcl-mode '(terragrunt hclfmt))))

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

(use-package erlang :ensure t :disabled t ; big boy that takes ages to clone
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

(use-package kdl-ts-mode :ensure (:host github :repo "merrickluo/kdl-ts-mode")
  ;; Major-mode for the KDL file format used by Zellij.
  :mode "\\.kdl\\'")

(use-package reader ; native deps--package injected via Nix
  ;; General-purpose document reader. Uses native modules with buffered
  ;; rendering to improve performance.
  :init
  (require 'reader-autoloads))

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
  (apheleia-formatters-respect-fill-column t)
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
  :custom
  (tempel-path (file-name-concat +templates-dir "*.eld"))
  :init
  (add-hook! '(prog-mode-hook text-mode-hook config-mode-hook)
    (add-hook 'completion-at-point-functions #'tempel-expand -90 t))

  :config
  (add-hook '+escape-hook
            (defun +tempel-esc-exit-h ()
              (when (tempel--active-p nil (current-buffer))
                (tempel-done)
                t))))

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

  (+define-file-template (rx "terragrunt.hcl" eos) "terragrunt/terragrunt.eld")
  (+define-file-template (rx "root.hcl" eos) "terragrunt/root.eld")
  (+define-file-template (rx "region.hcl" eos) "terragrunt/region.eld")
  (+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld")

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


;;; Wide World of Web

(use-package elfeed :ensure t)



(use-package claude-code-ide :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  ;; Run claude-code inside Emacs; creates an MCP bridge between the processes
  ;; to provide editor integration.
  :after-call +first-input-hook +first-file-hook
  :custom
  (claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-enable-mcp-server t)
  ;; disable ediff for proposed changes--too interruptive.
  (claude-code-ide-use-ide-diff nil)
  :config
  (claude-code-ide-emacs-tools-setup)
  (claude-code-ide-mcp-server-ensure-server)

  ;; Disable evil-mode in ancillary buffers using eat
  (with-eval-after-load 'evil
    (pushnew! evil-buffer-regexps `(,(rx bol "*claude-code"))))

  (define-advice claude-code-ide--create-terminal-session (:around (fn &rest args))
    "Teach claude-code to use the mise environment, when available"
    (let* ((mise-vars
            (when (and (executable-find "mise")
                       (locate-dominating-file default-directory ".mise.toml"))
              (string-split
               (replace-regexp-in-string (rx bol "export" (+ space))
                                         ""
                                         (shell-command-to-string "mise env"))
               "\n"
               t)))
           (process-environment (append mise-vars process-environment)))
      (apply fn args)))

  ;; Auto-scroll claude-code-ide buffers to bottom when switching tabs/windows
  (defun +claude-code-ide-scroll-to-bottom-h ()
    "Scroll all visible claude-code-ide buffers to bottom.
This ensures consistent positioning when switching tabs, frames, or windows."
    (dolist (window (window-list nil 'no-minibuffer))
      (with-selected-window window
        (when (and (buffer-live-p (current-buffer))
                   (string-match-p (rx bol "*claude-code") (buffer-name)))
          (goto-char (point-max))
          (recenter -1)))))

  (add-hook '+switch-window-hook #'+claude-code-ide-scroll-to-bottom-h)
  (add-hook '+switch-buffer-hook #'+claude-code-ide-scroll-to-bottom-h))

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
(put 'downcase-region 'disabled nil)
