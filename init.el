;;; init.el --- main Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the primary Emacs init file, loaded by the editor on startup. It is
;; loaded after early-init.el.

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

(require 'use-package)

(eval-and-compile
  (defvar +lisp-dir (file-name-concat user-emacs-directory "lisp/"))
  (add-to-list 'load-path +lisp-dir)
  (require '+corelib)
  (require '+load-incrementally))

(defvar org-directory "~/org/")
(defvar org-roam-directory "~/org/roam/")
(defvar org-default-notes-file "~/org/notes.org")

(defvar +site-files-directory (file-name-concat user-emacs-directory "site/"))
(defvar +templates-dir (file-name-concat user-emacs-directory "templates/"))
(defvar +ligatures-dir (file-name-concat user-emacs-directory "ligatures/"))


;;; Bootstrap Elpaca

;; TODO: Remove once Emacs 30 is out of pretest.
(when (eq emacs-major-version 30)
  (defvar elpaca-core-date 20241219))

(unless (featurep 'elpaca)
  (load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el")))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

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


;; Adapt the escape key customisation from Doom.

(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map))

(with-eval-after-load 'general
  (general-define-key :keymaps +default-minibuffer-maps
                      "S-v" #'yank))

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
(with-eval-after-load 'general
  (general-define-key :keymaps +default-minibuffer-maps [escape] #'+escape))
(with-eval-after-load 'eldoc
  (eldoc-add-command '+escape))


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
  (require '+edit-cmds)

  (general-define-key
   :states '(normal motion)
   :prefix "SPC"
   :prefix-command '+leader-key

   "SPC" '(consult-buffer :wk "buffers & files")
   "RET" '(gptel-menu :wk "LLM...")
   "." '(other-window-prefix :wk "in other window...")
   "!" '(async-shell-command :wk "shell command")
   "|" '(rotate-layout :wk "rotate window layout")
   "-" '(window-toggle-side-windows :wk "side windows")
   ":" '(pp-eval-expression :wk "eval")
   ";" '(ielm :wk "REPL")
   "d" '(dirvish :wk "dir editor")
   "i" '(consult-imenu :wk "imenu")
   "r" #'vertico-repeat
   "s" '(save-buffer :wk "save buf")
   "S" '(save-some-buffers :wk "save some bufs...")
   "u" '(universal-argument :wk "C-u")
   "x" '(execute-extended-command :wk "M-x")
   "K" 'man
   "T" (list (defun +goto-mode-template-file ()
               (interactive)
               (let* ((modes (nreverse (parent-mode-list major-mode)))
                      (mode (completing-read "Snippets table for mode: " modes nil t))
                      (filename (format "%s.eld" (string-remove-suffix "-mode" mode))))
                 (find-file (file-name-concat user-emacs-directory "templates" filename))))
             :wk "edit templates...")


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
         (and (derived-mode-p 'markdown-mode) (markdown-code-block-at-point-p)) 'markdown-edit-code-block)

   "/" '(consult-ripgrep :wk "search (rg)")
   "*" (list (defun +consult-ripgrep-symbol ()
               (interactive)
               (consult-ripgrep nil (format "%s" (symbol-at-point))))
             :wk "search (symbol)")

   "<tab>" (list (defun +swap-buffers ()
                   "Switch between the previous buffer and the current one."
                   (interactive)
                   (switch-to-buffer nil))
                 :wk "swap bufs")

   "p"  '(nil :wk "project")
   "p" project-prefix-map

   "h"  '(nil :wk "help")
   "h" help-map

   "," '(nil :wk "structure")
   ",n" '(puni-forward-sexp :wk "forward-sexp")
   ",p" '(puni-backward-sexp :wk "backward-sexp")
   ",<" '(puni-backward-sexp-or-up-list :wk "backward-sexp-or-up-list")
   ",c" '(puni-convolute :wk "convolute")
   ",d" '(+forward-kill-sexp :wk "kill sexp forward")
   ",D" '(+backward-kill-sexp :wk "kill sexp back")

   ",k" '(puni-splice-killing-forward :wk "splice-killing-forward")
   ",K" '(puni-splice-killing-backward :wk "splice-killing-backward")
   ;; TODO: define a killing-around variant.
   ",s" '(puni-splice-killing-backward :wk "splice-killing-backward")
   ",r" '(puni-raise :wk "raise")
   ",b" '(puni-barf-forward :wk "barf-forward")
   ",B" '(puni-barf-backward :wk "barf-backward")
   ",m" '(puni-slurp-forward :wk "slurp-forward")
   ",M" '(puni-slurp-backward :wk "slurp-backward")
   ",t" '(puni-transpose :wk "transpose")
   ",u" '(puni-splice :wk "splice")
   ",x" '(puni-split :wk "split")

   "a"  '(nil :wk "apps")
   "ac" #'quick-calc
   "aC" #'full-calc
   "ae" #'eshell
   "ar" (general-predicate-dispatch 'profiler-start
          (and (featurep 'profiler) (profiler-running-p)) #'+profiler-stop-and-report)

   "ap"  '(nil :wk "elpaca")
   "app" #'elpaca-manager
   "apl" #'elpaca-log
   "api" #'elpaca-info
   "apb" #'elpaca-browse
   "apv" #'elpaca-visit

   "b"  '(nil :wk "buffers")
   "bb" '(bury-buffer :wk "bury")
   "bd" '(bury-buffer :wk "bury")
   "bD" '(kill-current-buffer :wk "kill")
   "bl" '(ibuffer :wk "list")
   "bn" '(next-buffer :wk "next")
   "bp" '(previous-buffer :wk "prev")
   "bc" (list
         (general-predicate-dispatch #'clone-indirect-buffer
           (region-active-p) #'+clone-indirect-buffer-of-region)
         :wk "clone indirect")

   "f"  '(nil :wk "files")
   "ff" '(find-file :wk "find")
   "fF" '(find-file-other-window :wk "find (other window)")
   "fs" '(save-buffer :wk "save")
   "fR" '(rename-visited-file :wk "rename")
   "fr" '(recentf :wk "recent")
   "fw" '(write-file :wk "write copy")
   "fo" '(find-sibling-file :wk "other file")

   "fD" (list (defun +delete-file-and-buffer ()
                (interactive)
                (let ((file (buffer-file-name)))
                  (kill-buffer (current-buffer))
                  (when file
                    (delete-file file))))
              :wk "delete file & buf")

   "fy" (list (defun +copy-file-path ()
                (interactive)
                (if-let* ((file (buffer-file-name)))
                    (progn
                      (kill-new file)
                      (message "Copied to clipboard => %s" file))
                  (user-error "Buffer is not visiting a file")))
              :wk "copy (full path)")

   "fd" (list (defun +copy-file-directory ()
                (interactive)
                (if-let* ((file (buffer-file-name))
                          (dir (file-name-directory file)))
                    (progn
                      (kill-new dir)
                      (message "Copied to clipboard => %s" dir))
                  (user-error "Buffer is not visiting a file")))
              :wk "copy (dir)")

   "fv" (list (defun +revisit-file ()
                (interactive)
                (if-let* ((file (buffer-file-name)))
                    (find-alternate-file file)
                  (user-error "Buffer is not visiting a file")))
              :wk "reload")

   "n"  '(nil :wk "narrowing")
   "nf" '(narrow-to-defun :wk "defun")
   "nr" '(narrow-to-region :wk "region")
   "nw" #'widen

   "c"  '(nil :wk "code/comments")
   "cm" '(xref-find-references :wk "find refs")
   "cr" '(comment-dwim :wk "comment (dwim)")
   "cd" '(eglot-find-typeDefinition :wk "find type def")
   "cc" '(eglot-find-declaration :wk "find decl")
   "ci" '(eglot-find-implementation :wk "find impl")
   "cl" '(comment-line :wk "comment out")

   "g"  '(nil :wk "git/goto")
   "gb" '(magit-blame :wk "blame")
   "gd" '(magit-diff-buffer-file :wk "buffer diff")
   "gf" '(magit-file-dispatch :wk "file actions...")
   "gg" '(magit-status :wk "status")
   "gl" '(magit-log-buffer-file :wk "buffer log")
   "gr" '(browse-at-remote :wk "open on GitHub")
   "gt" '(git-timemachine-toggle :wk "file history")
   "gy" '(browse-at-remote-kill :wk "copy GitHub link ")

   "g?" (list (defun +goto-messages ()
                (interactive)
                (display-buffer "*Messages*"))
              :wk "messages")

   "ge" (list (defun +goto-emacs-init-file ()
                (interactive)
                (find-file (file-name-concat user-emacs-directory "init.el")))
              :wk "init file")

   "gs" (list (defun +goto-emacs-site-file ()
                (interactive)
                (find-file
                 (read-file-name "Site file: " +site-files-directory)))
              :wk "site file...")

   "gn" (list (defun +goto-nix-file ()
                (interactive)
                (project-find-file-in  "flake.nix" nil
                                       (project-current nil "~/.config/nix-configuration")))
              :wk "nix config file...")

   "L" '(gptel-menu :wk "LLM menu")
   "l" '(nil :wk "LLMs")
   "la" '(gptel-add :wk "add/remove from context")
   "lf" '(gptel-add-file :wk "add file to context")
   "ls" '(gptel-send :wk "send")
   "l?" '(gptel-menu :wk "menu")
   "ll" '(gptel :wk "open chat")
   "lw" '(gptel :wk "rewrite")

   "o"  '(nil :wk "org")
   "on" (list (defun +org-goto-notes ()
                (interactive)
                (find-file org-default-notes-file))
              :wk "notes")
   "oi" (list (defun +goto-org-roam-index ()
                (interactive)
                (find-file (file-name-concat org-roam-directory "notes/index.org")))
              :wk "roam index")
   "ot" (list (defun +goto-org-todos ()
                (interactive)
                (find-file (file-name-concat org-roam-directory "todos.org")))
              :wk "todos")
   "oa" (list (defun +org-agenda-dwim ()
                (interactive)
                (org-agenda nil "p"))
              :wk "agenda")

   "oj" '(consult-org-agenda :wk "agenda file heading...")
   "og" '(org-capture-goto-last-stored :wk "goto captured")
   "ov" '(org-tags-view :wk "search by tag")
   "ok" #'org-capture
   "ol" '(org-store-link :wk "store link")
   "of" '(+roam-node-find :wk "find (roam)")
   "os" '(org-roam-search :wk "search (roam)")
   "ow" '(timekeep-visit-node :wk "work file")

   "oc" '(nil :wk "clock")
   "occ" '(org-clock-in-last :wk "clock in (last)")
   "ocd" (list (general-predicate-dispatch #'org-clock-display
                 (not (derived-mode-p 'org-mode))
                 (defun +org-clock-display-last (&optional arg)
                   "Jump to the latest clock and display clocking info in that buffer."
                   (interactive "P")
                   (org-clock-goto arg)
                   (org-clock-display)))
               :wk "display")
   "oci" '(org-clock-in :wk "clock in")
   "oco" '(org-clock-out :wk "clock out")
   "ocr" '(org-resolve-clocks :wk "resolve")
   "ocg" '(org-clock-goto :wk "goto clock")
   "ocq" '(org-clock-cancel :wk "cancel")

   "or" '(nil :wk "roam/review")
   "ord" '(org-roam-review-list-recently-added :wk "list recent")
   "orl" '(org-roam-links :wk "linked nodes")
   "orr" '(org-roam-review :wk "review")
   "ort" '(org-roam-search-tags :wk "search by tag")

   "e"  '(nil :wk "errors")
   "el" '(consult-flymake :wk "error list")
   "en" '(next-error :wk "next error")
   "ep" '(previous-error :wk "prev error")

   "kr" '(consult-yank-pop :wk "kill-ring")

   "t"  '(nil :wk "toggles")
   "tb" '(breadcrumb-mode :wk "breadcrumbs (header)")
   "td" '(dirvish-side :wk "dirvish (side window)")
   "th" '(global-hl-line-mode :wk "highlight line")
   "tf" '(global-display-fill-column-indicator-mode :wk "fill column indicator")
   "ti" '(indent-bars-mode :wk "indent bars")
   "tl" '(global-display-line-numbers-mode :wk "line numbers")
   "tm" '(toggle-input-method :wk "input method")
   "ts" '(spell-fu-mode :wk "spellchecks")
   "tr" '(read-only-mode :wk "readonly")
   "tw" '(whitespace-mode :wk "whitespace")

   "w"  '(nil :wk "windows")
   "w-" '(+split-window-vertically-dwim :wk "vsplit")
   "w/" '(+split-window-horizontally-dwim :wk "hsplit")
   "w="  '(balance-windows :wk "balance")
   "wd" '(delete-window :wk "delete")
   "wo"  '(+delete-nondedicated-windows :wk "delete others")
   "wO"  '(delete-other-windows :wk "delete (+dedicated)")
   "wq" '(delete-window :wk "delete")
   "wr" '(evil-window-rotate-downwards :wk "rotate")
   "ws" '(consult-register-load :wk "registers")
   "wS" '(window-configuration-to-register :wk "save to reg")
   "wt"  '(+toggle-window-dedication :wk "toggle dedication")
   "ww" '(other-window :wk "other")

   "z" '(global-text-scale-adjust :wk "text scaling")
   )

  ;; Support multiple SPC-u calls in sequence to chain universal-argument calls.

  (keymap-set universal-argument-map "SPC u" #'universal-argument-more))

(defmacro +local-leader-set-key (keymaps &rest general-args)
  (declare (indent 1))
  `(general-define-key :prefix "," :states '(normal motion) :keymaps ,keymaps ,@general-args))


;;; General editing

(custom-theme-set-faces 'user
                        '(region ((t (:foreground unspecified :background unspecified :inherit modus-themes-search-lazy))))
                        '(iedit-occurrence ((t (:inherit modus-themes-search-replace))))
                        ;; Dim delimiters like commas, semicolons, etc.
                        '(font-lock-delimiter-face ((t (:inherit shadow)))))

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Since I use evil, I have no need for the usual rectangular selection
;; keybinding.
(when (equal system-type 'darwin)
  (keymap-global-set "C-x SPC" #'ns-do-show-character-palette))

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

(defvar +auto-save-dir (file-name-concat user-emacs-directory "autosave/"))

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
  (backup-directory-alist `(("." . ,+auto-save-dir)))
  (auto-save-list-file-prefix (file-name-concat +auto-save-dir ".saves-"))
  (auto-save-file-name-transforms `((".*" ,+auto-save-dir t)))

  ;; Used by `find-sibling-file' to figure out what files are related.
  (find-sibling-rules
   `(
     ;; Tests -> impl in TS
     ,(list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
            (rx (backref 1) ".ts"))

     ;; Impl -> tests in TS
     ,(list (rx (group (+? any)) ".ts" eos)
            (rx (backref 1) ".test.ts")
            (rx (backref 1) ".integration.ts"))
     ))

  :config
  (define-advice after-find-file (:around (fn &rest args) dont-block-on-autosave-exists)
    "Prevent the editor blocking to inform you when an autosave file exists."
    (cl-letf (((symbol-function #'sit-for) #'ignore))
      (apply fn args))))

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
  :general
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
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil) ; automatically save before compiling.
  (compilation-scroll-output 'first-error)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

  ;; Automatically truncate long compilation buffers.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

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
  :config
  (defun +profiler-stop-and-report (&optional continue-p)
    "Stop the profiler and show results.

With optional prefix arg CONTINUE-P, keep profiling."
    (interactive "P")
    (let ((ran-p (profiler-running-p)))

      (unless continue-p
        (profiler-stop))
      (profiler-report)
      (when ran-p
        (if continue-p
            (message "Profiler still recording")
          (message "Profiler stopped"))))))

(use-package goto-addr
  ;; Turns URLs in the buffer into clickable buttons.
  :init
  (defun +goto-address-maybe-h ()
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (goto-address)
      (goto-address-mode +1)))
  :hook ((prog-mode-hook text-mode-hook conf-mode-hook magit-process-mode-hook) . +goto-address-maybe-h)

  ;; Teach evil-ret to open URLs.
  :init
  (define-advice evil-ret (:before-until (&rest _) goto-addr)
    (when-let* ((url (thing-at-point 'url)))
      (browse-url url)
      t)))

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

;; TODO: Why is this bound on some installations but not others? 🤔
(when (boundp 'trusted-content)
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
  (add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
  (add-to-list 'trusted-content (file-name-concat +lisp-dir "lisp/")))

;; Silence "For information about GNU Emacs and the GNU system..." on startup.
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; Don't tell me what key I could have used instead of M-x.
(advice-add #'execute-extended-command--describe-binding-msg :override #'ignore)

(use-package eshell
  ;; Emacs' built-in shell combining Emacs Lisp evaluation with Unix shell
  ;; features.
  :config
  (require '+eshell))

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

(use-package ligature :ensure t
  ;; Teach Emacs how to display ligatures when available.
  :after-call +first-buffer-hook +first-file-hook
  :config

  (defun +read-ligatures (file)
    (with-temp-buffer
      (insert-file-contents-literally (file-name-concat +ligatures-dir file))
      (read (current-buffer))))

  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode (+read-ligatures "prog-mode.eld"))
  (ligature-set-ligatures '(text-mode org-agenda-mode) (+read-ligatures "text-mode.eld"))

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
  (setq-default comment-column 0)
  (with-eval-after-load 'evil

    ;; Teach "J" (evil-join) to delete comment delimiters as needed to join
    ;; lines.

    ;; Taken from doom, which itself adapts solutions in this github issue:
    ;; https://github.com/emacs-evil/evil/issues/606

    (define-advice evil-join (:around (fn beg end) join-comments)
      (if-let* (((not (= (line-end-position) (point-max))))
                (cend (save-excursion (goto-char end) (line-end-position)))
                (cbeg (save-excursion
                        (goto-char beg)
                        (and (+point-in-comment-p
                              (save-excursion
                                (goto-char (line-beginning-position 2))
                                (skip-syntax-forward " \t")
                                (point)))
                             (or (comment-search-backward (line-beginning-position) t)
                                 (comment-search-forward  (line-end-position) t)
                                 (and (+point-in-comment-p beg)
                                      (stringp comment-continue)
                                      (or (search-forward comment-continue (line-end-position) t)
                                          beg)))))))
          (let* ((count (count-lines beg end))
                 (count (if (> count 1) (1- count) count))
                 (fixup-mark (make-marker)))
            (uncomment-region (line-beginning-position 2)
                              (save-excursion
                                (goto-char cend)
                                (line-end-position 0)))
            (unwind-protect
                (dotimes (_ count)
                  (join-line 1)
                  (save-match-data
                    (when (or (and comment-continue
                                   (not (string-empty-p comment-continue))
                                   (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                              (and comment-start-skip
                                   (not (string-empty-p comment-start-skip))
                                   (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                      (replace-match "" t nil nil 1)
                      (just-one-space))))
              (set-marker fixup-mark nil)))
        ;; But revert to the default we're not in a comment, where
        ;; `fill-region-as-paragraph' is too greedy.
        (funcall fn beg end)))))

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

(use-package hideshow
  ;; Basic code folding.
  :hook (prog-mode-hook . hs-minor-mode))

(use-package page-break-lines :ensure t
  ;; Displays ^L page break characters as a horizontal rule. Useful for
  ;; demarcating sections of a file.
  :after-call +first-file-hook +first-buffer-hook
  :config
  (global-page-break-lines-mode +1))

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
          gptel-mode-hook
          org-roam-mode-hook
          )
         . hide-mode-line-mode))

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
     ("NOTE" success bold))))

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
  (require '+pulsar)

  (add-hook 'next-error-hook #'pulsar-pulse-line)

  (dolist (hook '(consult-after-jump-hook
                  imenu-after-jump-hook))
    (add-hook hook #'pulsar-recenter-top)
    (add-hook hook #'pulsar-reveal-entry))

  (dolist (hook '(org-agenda-after-show-hook
                  org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-center)
    (add-hook hook #'pulsar-reveal-entry))

  (define-advice flymake-goto-next-error (:after (&rest _) pulsar)
    (when pulsar-mode
      (pcase (cl-loop for o in (overlays-at (point))
                      for diag = (overlay-get o 'flymake-diagnostic)
                      when diag
                      return (flymake--severity (flymake-diagnostic-type diag)))
        (3 (pulsar-pulse-line-red))
        (2 (pulsar-pulse-line-yellow))
        (_ (pulsar-pulse-line-cyan)))))

  (delq! 'evil-goto-first-line pulsar-pulse-functions)
  (delq! 'evil-goto-line pulsar-pulse-functions)

  (define-advice evil-goto-line (:after (count) pulsar)
    "Don't pulse if moving to the first or last line via gg/G."
    (when (and pulsar-mode
               count ; nil if going to end of buffer
               (< 1 count ))
      (pulsar-pulse-line)))

  (define-advice evil-yank (:after (start end &rest _) pulsar)
    "Pulse yanked lines & regions."
    (when pulsar-mode
      (pulsar--pulse nil 'pulsar-generic start end)))

  (define-advice evil-jump-item (:after (&rest _) pulsar)
    "Pulse if jumping to a different line."
    (unless (region-active-p)
      (pulsar-pulse-line)))

  ;; Show a pulse indicating success or failure of eval-expression, eval-region,
  ;; etc.
  :config
  (define-advice eval-region (:around (fn start end &rest args) pulsar)
    "Pulse evaluated regions."
    (+with-eval-pulse start end
      (apply fn start end args)))

  (define-advice eval-last-sexp (:around (fn &rest args) pulsar)
    "Pulse evaluated expressions."
    (pcase-let ((`(,start . ,end) (or (bounds-of-thing-at-point 'sexp)
                                      (cons (ignore-errors (save-excursion
                                                             (backward-sexp)
                                                             (point)))
                                            (point)))))
      (+with-eval-pulse start end
        (apply fn args)))))

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
  (show-paren-context-when-offscreen 'overlay))

(use-package paren-face :ensure t
  ;; Adds a face for parentheses in lisps. I hijack it to dim semicolons and
  ;; other non-critical syntax elements in other langs.
  :hook (lisp-data-mode-hook . paren-face-mode))

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
  (ispell-set-spellchecker-params))

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


;;; Dired & dirvish

(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-auto-revert-buffer 'dired-directory-changed-p)
  (dired-listing-switches
   "--almost-all --human-readable --group-directories-first --no-group"))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-vc-rename-file t))

(use-package nerd-icons :ensure t
  ;; Icon set used by various packages.
  :autoload nerd-icons-codicon nerd-icons-faicon)

(use-package wdired
  ;; Makes dired buffers directly editable; the changes are interpreted into
  ;; operations on the corresponding file names.
  :general
  (:keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))

(use-package dirvish :ensure t
  ;; Wrapper around `dired' that provides better UX.
  :hook (+first-input-hook . dirvish-override-dired-mode)

  :general
  (:keymaps '(dirvish-mode-map dired-mode-map) :states 'normal
            "q" #'dirvish-quit)
  (:keymaps 'dirvish-mode-map :states 'normal
            "<tab>" #'dirvish-layout-toggle)
  :custom
  (dirvish-reuse-session nil)
  (dirvish-attributes '(file-size nerd-icons subtree-state))
  (dirvish-mode-line-format '(:left (sort file-time symlink) :right (omit yank index)))
  (dirvish-subtree-always-show-state t)
  (dirvish-hide-details '(dirvish dirvish-side))
  (dirvish-hide-cursor '(dirvish dirvish-side))

  :config
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))

  :config
  (dirvish-peek-mode +1))

(use-package diredfl :ensure t
  ;; Add extra font-lock to dired/dirvish file listings.
  :hook ((dired-mode-hook dirvish-directory-view-mode-hook) . diredfl-mode))


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
  (:states 'insert "RET" 'comment-indent-new-line)
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

  ;; Cursor customisation
  :config
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor  'hollow)
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
  (evil-mode +1)

  ;; Use more natural Emacs/readline keybindings in ex.
  :general-config
  (:keymaps '(evil-ex-completion-map evil-ex-search-keymap)
            "C-a" #'evil-beginning-of-line
            "C-b" #'evil-backward-char)

  :config
  (defun +delete-backward-word-no-kill (arg)
    "Like `backward-kill-word', but doesn't affect the kill-ring."
    (interactive "p")
    (let ((kill-ring nil) (kill-ring-yank-pointer nil))
      (ignore-errors (backward-kill-word arg))))

  :general-config
  (:keymaps +default-minibuffer-maps
            "C-a"    #'move-beginning-of-line
            "C-r"    #'evil-paste-from-register
            "C-u"    #'evil-delete-back-to-indentation
            "C-v"    #'yank
            "C-w"    #'+delete-backward-word-no-kill))

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
  (add-hook! 'emacs-lisp-mode-hook
    (make-local-variable 'evil-surround-pairs-alist)
    (setf (alist-get ?` evil-surround-pairs-alist) '("`" . "'"))
    (setf (alist-get ?f evil-surround-pairs-alist) 'evil-surround-prefix-function)))

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

  ;; Customise the action keys to make actions a bit more vimmy.
  :config
  (require '+avy)
  :custom
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
                        (? . avy-action-zap-to-char)))

  ;; Integrate avy with pulsar for better visual feedback

  :config
  (with-eval-after-load 'pulsar
    (define-advice avy-process (:filter-return (result) pulse-red-on-no-matches)
      (when (eq t result)
        (when pulsar-mode
          (pulsar-pulse-line-red)))
      result)

    (defun +avy-pulse-for-move (&rest _)
      (when pulsar-mode
        (pulsar-pulse-line)))

    (advice-add #'avy-action-goto :after #'+avy-pulse-for-move)

    (defun +avy-pulse-for-change (&rest _)
      (when pulsar-mode
        (pulsar-pulse-line-magenta)))

    (advice-add #'avy-action-kill-move :after #'+avy-pulse-for-change)
    (advice-add #'+avy-action-change-move :after #'+avy-pulse-for-change)

    (defun +avy-pulse-for-change-elsewhere (fn pt)
      (+with-clean-up-in-starting-buffer-and-window (funcall fn pt)
        (when pulsar-mode
          (goto-char pt)
          (pulsar-pulse-line-magenta))))

    (advice-add #'avy-action-kill-stay :around #'+avy-pulse-for-change-elsewhere)

    (defun +avy-pulse-for-action-elsewhere (fn pt)
      (+with-clean-up-in-starting-buffer-and-window (funcall fn pt)
        (when pulsar-mode
          (goto-char pt)
          (pulsar-pulse-line-green))))

    (advice-add #'+avy-action-evil-lookup :around #'+avy-pulse-for-action-elsewhere)
    (advice-add #'avy-action-copy :around #'+avy-pulse-for-action-elsewhere)
    (advice-add #'avy-action-ispell :around #'+avy-pulse-for-action-elsewhere))

  ;; KLUDGE: Pre-configure indentation for dynamically-loaded macro. Ensures
  ;; Apheleia applies correct indentation if I touch this file without avy being
  ;; loaded in the editing session.
  :init
  (function-put '+with-clean-up-in-starting-buffer-and-window 'lisp-indent-function 1))

;; Use +/- to mark syntactic elements with tree-sitter. However, if I don't have
;; a selection, make - call avy.
(general-define-key :states '(normal motion)
                    "-" (general-predicate-dispatch #'avy-goto-char-timer
                          (region-active-p) #'expreg-contract)
                    "+" #'+expreg-expand-dwim)

(use-package ace-window :ensure t
  ;; Jump to specific windows
  :general ("M-o" #'ace-window))


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
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)

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
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
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
  :custom
  (transient-display-buffer-action '(display-buffer-below-selected))
  :general-config
  (:keymaps 'transient-map [escape] #'transient-quit-one))

(use-package magit :ensure t
  ;; Magit is the definitive UX for working with git.
  :config
  ;; Set initial evil state depending on whether the line is empty or not. Empty
  ;; line = new commit message, whereas non-empty means we're editing an
  ;; existing one.
  (add-hook! 'git-commit-mode-hook
    (when (and (bolp) (eolp))
      (evil-insert-state)))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
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

  :preface
  (setq forge-database-file (file-name-concat user-emacs-directory "forge/forge-database.sqlite"))
  :general
  (:keymaps 'magit-mode-map [remap magit-browse-thing] #'forge-browse)
  (:keymaps 'magit-remote-section-map [remap magit-browse-thing] #'forge-browse-remote)
  (:keymaps 'magit-branch-section-map [remap magit-browse-thing] #'forge-browse-branch)
  :general-config
  (:keymaps 'forge-topic-list-mode-map :states 'normal "q" #'kill-current-buffer))

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
  (gac-silent-message-p t))


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
  (project-switch-commands
   (defun +project-switch-magit-status ()
     (interactive)
     (let* ((proj (project-current t))
            (root (project-root proj)))
       (if (file-directory-p (file-name-concat root ".git"))
           (magit-status-setup-buffer root)
         (dirvish root))))))


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
  :general (:keymaps 'help-mode-map :states 'normal "^" #'help-go-back))

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
  :general-config (:keymaps 'emacs-lisp-mode-map "C-c RET" #'pp-macroexpand-last-sexp)

  :config
  (defun +emacs-lisp-lookup-func ()
    (describe-symbol (symbol-at-point)))

  (+local-leader-set-key 'emacs-lisp-mode-map
    "e" '(nil :which-key "eval")
    "eb" (defun +eval-buffer ()
           (interactive)
           (let ((inhibit-redisplay t))
             (call-interactively #'eval-buffer)
             (message "Buffer evaluated" ))
           (when pulsar-mode
             (pulsar--pulse nil 'pulsar-yellow (point-min) (point-max)))))

  (defun +elisp-set-flymake-load-path ()
    (if-let* ((file (buffer-file-name))
              (this-dir (expand-file-name (file-name-directory file)))
              (emacs-config-dirs (seq-map #'expand-file-name
                                          (list user-emacs-directory
                                                +lisp-dir))))
        (if (member this-dir emacs-config-dirs)
            load-path
          '("./"))))

  (setq-hook! 'emacs-lisp-mode-hook
    elisp-flymake-byte-compile-load-path (+elisp-set-flymake-load-path)
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
    (advice-add #'calculate-lisp-indent :override #'+elisp--calculate-lisp-indent-a))

  :custom
  (define-advice eval-region (:around (fn &rest args) clear-visual-state)
    (unwind-protect (apply fn args)
      (when (eq evil-state 'visual)
        (evil-normal-state)))))

(use-package nix-ts-mode :ensure t
  :mode "\\.nix\\'"
  :config
  (setq-hook! 'nix-ts-mode-hook apheleia-formatter 'nixpkgs-fmt)
  (with-eval-after-load 'apheleia
    (add-to-list 'apheleia-formatters '(nixpkgs-fmt "nixpkgs-fmt"))))

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
  :mode ("rc\\'")
  )

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
  :general
  (:keymaps 'eglot-mode-map
   :states '(insert normal)
   "M-RET" #'eglot-code-actions)
  (:keymaps 'eglot-mode-map
   :states '(normal)
   "C-c C-r" #'eglot-rename))

(use-package markdown
  :custom
  (markdown-fontify-code-blocks-natively t))

;; Use tree-sitter modes.

(alist-set! major-mode-remap-alist #'c-mode #'c-ts-mode)


;;; Debuggers

(use-package debug
  ;; The built-in debugger for the Emacs Lisp runtime.
  :init
  (defun +debugger-toggle-on-exit-frame ()
    (interactive)
    (let ((enabled-for-line (save-excursion
                              (goto-char (line-beginning-position))
                              (looking-at (rx (* space) "*" (+ space))))))
      (cond
       (enabled-for-line
        (debugger-frame-clear)
        (message "debug on exit for frame disabled"))
       (t
        (debugger-frame)
        (message "debug on exit for frame enabled")))))

  :general
  (:keymaps 'debugger-mode-map :states 'normal "t" #'+debugger-toggle-on-exit-frame)

  :config
  (define-advice debugger-record-expression (:after (&rest _) display-buffer)
    (display-buffer debugger-record-buffer))

  ;; Show keybindings in the header line for Backtrace buffers.

  (defconst +debugger-mode-line-format
    (cl-labels ((low-emphasis (str)
                  (propertize str 'face 'parenthesis))
                (key (key desc)
                  (concat (propertize key 'face 'which-key-key-face) (low-emphasis ":") " " desc))
                (group (&rest children)
                  (concat (low-emphasis "|") "  " (apply #'distribute children)))
                (distribute (&rest strs)
                  (string-join strs "  ")))
      (distribute
       (propertize "  " 'face 'font-lock-builtin-face)
       (group
        (key "d" "step")
        (key "c" "continue")
        (key "r" "return"))
       (group
        (key "t" "toggle debug on exit frame")
        (key "J" "jump")
        (key "L" "locals"))
       (group
        (key "E" "eval")
        (key "R" "eval & record")))))

  (setq-hook! 'debugger-mode-hook
    mode-line-format +debugger-mode-line-format))


;;; Code formatting

(use-package apheleia :ensure t
  ;; Apply code formatting on save. Works for a range of languages.
  :after-call +first-file-hook
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
  :init
  (defmacro +define-file-template (mode-or-regexp template-file-name)
    (let ((template-file (file-name-concat auto-insert-directory template-file-name)))
      `(define-auto-insert ,mode-or-regexp (lambda ()
                                             (skeleton-insert
                                              (with-temp-buffer
                                                (insert-file-contents ,template-file)
                                                (read (buffer-string))))))))
  :config
  (auto-insert-mode +1)
  (+define-file-template (rx ".el" eos) "emacs-lisp.eld"))


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

  :custom
  (abbrev-file-name (file-name-concat org-directory "abbrev.el"))

  ;; visual settings
  (org-list-indent-offset 1)
  (org-cycle-separator-lines 0)
  (org-ellipsis " …")
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-startup-shrink-all-tables t)
  (org-startup-with-inline-images t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-indent-indentation-per-level 3)
  (org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . success)))

  (org-tags-column 0)
  (org-auto-align-tags nil)
  (org-catch-invisible-edits 'show-and-error)

  ;; TODOs, checkboxes, stats, properties.
  (org-todo-keywords '((type "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "CANCELLED(c@)")
                       (type "PROJECT(p)" "|" "DONE(d!)")))
  (org-use-fast-todo-selection 'expert)
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-hierarchical-todo-statistics nil)
  (org-use-property-inheritance t)
  (org-log-into-drawer t)

  ;; babel & src support
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation nil)
  (org-src-window-setup 'plain)
  (org-confirm-babel-evaluate nil)
  (org-link-elisp-confirm-function nil)
  (org-babel-default-header-args:emacs-lisp '((:lexical . "yes")))
  (org-babel-python-command "python3")
  (org-babel-load-languages '((emacs-lisp . t)
                              (C . t)
                              (calc . t)
                              (shell . t)))

  ;; interactive behaviour
  (org-imenu-depth 5)
  (org-bookmark-names-plist nil)
  (org-M-RET-may-split-line nil)
  (org-footnote-auto-adjust t)
  (org-insert-heading-respect-content t)
  (org-loop-over-headlines-in-active-region 'start-level)
  (org-return-follows-link t)
  (org-track-ordered-property-with-tag t)

  ;; Custom links

  (org-link-abbrev-alist
   '(("github"      . "https://github.com/%s")
     ("youtube"     . "https://youtube.com/watch?v=%s")
     ("google"      . "https://google.com/search?q=")
     ("wikipedia"   . "https://en.wikipedia.org/wiki/%s")))

  :config

  ;; Prefer inserting headings with M-RET
  (add-hook! 'org-metareturn-hook
    (when (org-in-item-p)
      (org-insert-heading current-prefix-arg)
      (evil-append-line 1)
      t))

  ;; Prevent flickering when org-indent is enabled.
  (setq-hook! 'org-mode-hook show-paren-mode nil)

  ;; Increase padding for readability.
  (setq-hook! '(org-mode-hook org-agenda-mode-hook) line-spacing 0.1)

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
  (advice-add #'org-highlight-new-match :override #'ignore)

  :config
  (define-advice org-return (:after (&optional indent _arg _interactive) emulate-major-mode-indent)
    "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
    (when (and indent org-src-tab-acts-natively (org-in-src-block-p t))
      (save-window-excursion
        (org-babel-do-in-edit-buffer
         (call-interactively #'indent-for-tab-command)))))

  ;; Make C-c C-k either cut subtrees or cancel open notes.
  :config
  (defun +org-cut-subtree-or-cancel-note ()
    (interactive)
    (cond (org-finish-function
           (let ((org-note-abort t)) (funcall org-finish-function)))
          ((bound-and-true-p org-capture-mode)
           (org-capture-kill))
          (t
           (org-cut-subtree))))

  ;; Highlight broken file links.
  (org-link-set-parameters
   "file" :face (lambda (path)
                  (if (or (file-remote-p path)
                          (file-exists-p path))
                      'org-link
                    '(warning org-link))))

  :general-config
  (:keymaps 'org-mode-map
   :states '(normal insert)
   "C-c C-k" #'+org-cut-subtree-or-cancel-note
   "C-c f" #'org-footnote-new
   "M-+" #'org-table-insert-column
   "M--" #'org-table-delete-column
   "C-c C-." #'org-time-stamp-inactive
   "C-c ." #'org-time-stamp
   "C-c RET" (general-predicate-dispatch #'org-insert-todo-heading
               (org-at-table-p) #'org-table-hline-and-move))

  (:keymaps 'org-mode-map
   :states 'normal
   "RET" 'org-open-at-point
   "M-p" #'org-metaup
   "M-n" #'org-metadown
   "C-c c" #'org-columns
   "C-c d" #'org-dynamic-block-insert-dblock
   "C-c n" 'org-next-link
   "C-c p" 'org-previous-link
   "C-c o" 'org-table-toggle-coordinate-overlays
   "SPC n s" #'org-narrow-to-subtree)

  :config
  (+local-leader-set-key 'org-mode-map
    "A" #'org-archive-subtree
    "I" #'org-id-get-create
    "y" #'org-copy-subtree
    "x" #'org-cut-subtree
    "p" #'org-paste-subtree
    "t" #'org-show-todo-tree)

  (custom-theme-set-faces 'user
                          '(org-footnote ((t (:underline nil))))))

(use-package ol
  ;; Hyperlink functionality in org-mode
  :config
  (defface +org-id-link
    '((t (:weight semi-bold :inherit font-lock-variable-name-face)))
    "Face for ID links; these would typically be org-roam links.")

  (org-link-set-parameters "id" :face '+org-id-link)

  (require 'ol-man))

(use-package org-capture
  ;; Implements templated information capture into org-mode files.
  :custom
  (org-capture-templates
   (cl-flet ((notes-datetree (key desc template &rest kvps)
               (append
                (list key desc 'entry '(file+olp+datetree org-default-notes-file) template)
                '(:tree-type (month day))
                kvps)))
     (list (notes-datetree "t" "Todo" "* TODO %?")
           (notes-datetree "n" "Note" "* %T %?")
           (notes-datetree "N" "Note (setting time)" "* %^T %?")

           '("w" "work")
           (notes-datetree "wt" "Todo" "* TODO %?               :%(timekeep-work-tag):work:")
           (notes-datetree "ww" "Note" "* %T %?                 :%(timekeep-work-tag):work:")
           (notes-datetree "wW" "Note (setting time)" "* %^T %? :%(timekeep-work-tag):work:")

           (notes-datetree "l" "Link" "* %T %(org-cliplink-capture)\n%?")
           (notes-datetree "p" "Postmortem" "* %T %? :pm:
** Description
# What happened?
** Background & Context
# What was the context? What was I doing when this happened, and what
# contributed to this?
** Timeline
# What happened? Give a play-by-play.
** Effects
# What were the consequences or after-effects?
** Mitigations
# What could I do to lessen the severity or chance of this happening again?
"
                           :jump-to-captured t))))
  :config
  (org-capture-put :kill-buffer t))

(use-package org-refile
  ;; Move org-mode headings and their contents around in a structured way,
  ;; both within the current file and to others.
  :custom
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :config
  ;; When refiling from org-capture, Emacs prompts to kill the underlying,
  ;; modified buffer. This fixes that.
  (add-hook 'org-after-refile-insert-hook #'save-buffer))

(defvar +org-habit-graph-window-ratio 0.2
  "The ratio of the consistency graphs relative to the window width.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph.")

(defvar +org-habit-min-width 30
  "Hide the consistency graph if `org-habit-graph-column' is less than this.")

(use-package org-habit
  ;; Declare certain tasks in the agenda as 'habits'; these have a graph
  ;; displayed beside them to help visualise your consistency.
  :after-call org-agenda
  :custom
  (org-habit-graph-column 72)
  (org-habit-today-glyph ?▲)
  (org-habit-completed-glyph ?✓)
  :config
  (add-hook 'org-agenda-mode-hook
            (defun +org-habit-resize-graph-h ()
              "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
              (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
                     (preceding-days-ratio (/ org-habit-preceding-days total-days))
                     (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
                     (preceding-days (floor (* graph-width preceding-days-ratio)))
                     (following-days (- graph-width preceding-days))
                     (graph-column (- (window-width) (+ preceding-days following-days)))
                     (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                                (- graph-column +org-habit-graph-padding)
                                              nil)))
                (setq-local org-habit-preceding-days preceding-days)
                (setq-local org-habit-following-days following-days)
                (setq-local org-habit-graph-column graph-column-adjusted)))))

(use-package org-clock
  ;; Provides time-tracking support for org-mode.
  :init
  (add-transient-hook! 'org-mode-hook #'org-clock-persistence-insinuate)
  :custom
  (org-clock-persist t))

(use-package org-src
  ;; Programming language source blocks.
  :config
  ;; TODO: Remap mode definitions so I don't have to maintain this
  ;; myself...
  (pushnew! org-src-lang-modes
            '("cs" . csharp-ts)
            '("csharp" . csharp-ts)
            '("docker" . dockerfile-ts)
            '("dockerfile" . dockerfile-ts)
            '("json" . json-ts)
            '("md" . markdown)
            '("nix" . nix-ts)
            '("rs" . rust-ts)
            '("rust" . rust-ts)
            '("sh" . bash-ts)
            '("ts" . typescript-ts)
            '("typescript" . typescript-ts)
            '("yaml" . yaml-ts)
            '("yml" . yaml-ts)
            ))

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

(use-package org-agenda
  ;; Aggregate TODOs and other tasks declared in org-mode files and display in a
  ;; consolidated dashboard.
  :general
  ("C-c a" #'org-agenda)
  :general-config
  (:keymaps 'org-agenda-mode-map :states 'motion
            "J" #'org-agenda-goto-date
            [remap save-buffer] #'org-save-all-org-buffers
            "C-n" #'org-agenda-later
            "C-p" #'org-agenda-earlier)
  :custom
  (org-agenda-files (file-name-concat org-directory "org-agenda-files"))
  (org-agenda-text-search-extra-files `(agenda-archives ,(file-name-concat org-directory "archive.org")))
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-search-view-always-boolean t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-tags-column 0)
  (org-archive-tag "ARCHIVED")
  (org-agenda-inhibit-startup nil)
  (org-agenda-custom-commands
   (let ((today '(agenda ""
                  ((org-agenda-overriding-header "Agenda")
                   (org-agenda-use-time-grid t)
                   (org-agenda-clockreport-parameter-plist '(:compact t
                                                             :link t
                                                             :maxlevel 3
                                                             :fileskip0 t
                                                             :filetitle t))
                   (org-agenda-show-inherited-tags t)
                   (org-agenda-skip-function #'+agenda-view-skip-function)
                   (org-super-agenda-groups
                    `((:name "Agenda" :time-grid t)
                      (:name "Forming" :and (:habit t :regexp ,(rx "->")))
                      (:name "French Study" :category "french")
                      (:name "Habits" :habit t)
                      (:name "Birthdays" :category "birthdays")
                      (:name "Delegated" :todo "WAIT")
                      (:name "Tickler" :tag "tickler"))))))

         (next-actions '(tags-todo "-tickler-inbox+TODO=\"TODO\""
                         ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-dim-blocked-tasks 'invisible)
                          (org-agenda-skip-function #'+agenda-next-actions-skip-function))))

         (inbox '(tags-todo "+inbox+TODO=\"TODO\""
                  ((org-agenda-overriding-header "Inbox")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))

         (delegated '(todo "WAIT"
                      ((org-agenda-overriding-header "Delegated")
                       (org-agenda-remove-tags nil)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled)))))

         (projects '(tags-todo "+TODO=\"PROJECT\""
                     ((org-agenda-overriding-header "Projects"))))

         (defaults `((org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-span 'day)
                     (org-agenda-window-setup 'only-window)
                     (org-agenda-start-day nil)
                     (org-agenda-include-diary nil)
                     (org-agenda-remove-tags t)
                     (org-agenda-insert-diary-extract-time nil)
                     (org-agenda-show-inherited-tags nil)
                     (org-agenda-skip-deadline-if-done t)
                     (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
                     (org-agenda-skip-scheduled-if-done t)
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-dim-blocked-tasks t)
                     (org-agenda-sorting-strategy '((agenda time-up habit-up priority-down category-up priority-down todo-state-up)
                                                    (todo priority-down category-up scheduled-up)
                                                    (tags priority-down category-up)
                                                    (search category-up)))
                     (org-agenda-clock-report-header "\nClocking")
                     (org-agenda-use-time-grid nil)
                     (org-agenda-show-future-repeats nil)
                     (org-agenda-ignore-properties '(effort appt)))))

     `(("p" "personal agenda" ,(list today next-actions inbox delegated projects)
        (,@defaults
         (org-agenda-tag-filter-preset '("-work" "-ignore"))))
       ("w" "work agenda" ,(list today next-actions inbox delegated projects)
        (,@defaults
         (org-agenda-tag-filter-preset (list "-ignore" (format "+%s" (timekeep-work-tag))))
         (org-agenda-clock-consistency-checks
          '(:gap-ok-around ("12:20" "12:40" "4:00")
            :max-duration "10:00"
            :min-duration 0
            :max-gap 0)))))))
  :config
  (require '+agenda)

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

  (add-hook! 'org-mode-hook
    (add-hook 'after-save-hook #'+org-agenda-update-files nil t))

  ;; Reveal context around item on TAB
  (add-hook! 'org-agenda-after-show-hook
    (org-overview)
    (org-reveal)
    (org-fold-show-subtree)
    (org-display-outline-path))
  )

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

(use-package org-archive
  ;; org-mode's glacier storage tier.
  :custom
  (org-archive-subtree-add-inherited-tags t)
  (org-archive-location (file-name-concat org-directory "archive.org::datetree/")))

(use-package org-roam :ensure t
  ;; Provides workflows for working with documents for atomic notes (e.g. a
  ;; Zettelkasten); implements backlinks between documents for discovering
  ;; connected notes.
  :after org

  :defer-incrementally
  ansi-color dash f rx seq magit-section emacsql

  :general-config
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'org-roam-node-insert)

  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))
  (org-roam-extract-new-file-path "notes/${slug}.org")
  (org-roam-completion-everywhere t)
  ;; prefer faster utilities
  (org-roam-list-files-commands '(fd fdfind rg find))

  :config
  (org-roam-db-autosync-mode +1)

  (+local-leader-set-key 'org-mode-map
    "<tab>" #'org-roam-buffer-toggle
    "l" '(nil :wk "aliases")
    "la" #'org-roam-alias-add
    "lx" #'org-roam-alias-remove)

  (setq-hook! 'org-roam-find-file-hook
    org-id-link-to-org-use-id 'create-if-interactive)

  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)

  ;; Work around clashes with evil bindings.
  :config
  (add-hook! 'org-roam-mode-hook (set-keymap-parent org-roam-mode-map nil))
  :general-config
  (:keymaps 'org-roam-mode-map
   "M-p"     #'magit-section-backward-sibling
   "M-n"     #'magit-section-forward-sibling
   [tab]     #'magit-section-toggle
   [C-tab]   #'magit-section-cycle
   [backtab] #'magit-section-cycle-global
   :states '(normal visual)
   "]"       #'magit-section-forward-sibling
   "["       #'magit-section-backward-sibling
   "gj"      #'magit-section-forward-sibling
   "gk"      #'magit-section-backward-sibling
   "gr"      #'revert-buffer
   "gR"      #'revert-buffer
   "z1"      #'magit-section-show-level-1
   "z2"      #'magit-section-show-level-2
   "z3"      #'magit-section-show-level-3
   "z4"      #'magit-section-show-level-4
   "za"      #'magit-section-toggle
   "zc"      #'magit-section-hide
   "zC"      #'magit-section-hide-children
   "zo"      #'magit-section-show
   "zO"      #'magit-section-show-children
   "zm"      #'magit-section-show-level-2-all
   "zr"      #'magit-section-show-level-4-all
   "C-j"     #'magit-section-forward
   "C-k"     #'magit-section-backward))

(use-package poporg :ensure t
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
   `(("src" . ("" "◌"))
     ("quote" . ("" ""))
     ("example" . ("" "◌"))))
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
  :config
  (setf (car calendar-time-display-form) '24-hours))


;;; Input methods

(setq default-input-method "french-postfix")
(setq default-transient-input-method default-input-method)

(keymap-global-set "M-i" 'activate-transient-input-method)

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



(use-package gptel :ensure t
  ;; Provides LLM integrations.
  :hook (gptel-mode-hook . visual-line-mode)
  :general
  (:keymaps 'gptel-mode-map :states '(normal insert) "C-c C-s" #'gptel-send)
  ("C-c s" #'gptel-menu)
  ("C-c C-s" #'gptel-send)
  :custom
  (gptel-model 'claude-3-7-sonnet-20250219)
  (gptel-default-mode 'org-mode)
  :config
  (alist-set! gptel-prompt-prefix-alist 'org-mode "* ")
  (setq-hook! 'gptel-mode-hook
    org-pretty-entities-include-sub-superscripts nil)

  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (lambda ()
                 (auth-source-pick-first-password :host "api.anthropic.com"))))

  (add-hook 'gptel-mode-hook 'evil-insert-state)

  ;; Prevent transient from creating extra windows due to conflicts with custom
  ;; display-buffer rules. See:
  ;; https://github.com/magit/transient/discussions/358
  (setq transient-display-buffer-action
        '(display-buffer-below-selected
          (dedicated . t)
          (inhibit-same-window . t)))
  )

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


;;; The Dark Pit of Display Buffer and Despair

;; Emacs' window management system is designed around the 'principle of most
;; surprise'. As you edit, Emacs wants to show you buffers, but how it choses
;; which window to display that buffer feels totally unpredictable.

;; Sometimes Emacs pops up a new one. Sometimes it re-uses an existing one. Why
;; did it pick that one? I dunno, it depends on each individual command how it
;; implemented buffer display. Sometimes you can quit the window with 'q';
;; sometimes you can't. Sometimes it will save & restore your window state on
;; quit, sometimes it scrambles it. Occasionally it will even use a *new frame*
;; (shudder).

;; You can improve the situation with `display-buffer-alist' and a lot of elbow
;; grease, but Emacs has the spirit of a wild stallion that can never be truly
;; tamed.

;; When I'm editing, I generally want a single main buffer to focus on, or two
;; displayed side-by-side. Sometimes it makes sense to pop up another window for
;; a short time, e.g. when looking up docs, running a couple of shell commands,
;; or doing a compilation. I teach `display-buffer' to use side windows for
;; these buffers.

(setq display-buffer-alist
      (cl-labels ((mode-active-p (mode)
                    (cl-assert (symbolp mode))
                    (lambda (buf _action)
                      (with-current-buffer buf
                        (and (boundp mode) (eval mode))))))
        (append

         ;; Top side - debugger stacks

         (cl-labels ((top (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (dedicated . t)
                                    (window-height . 0.2)
                                    (side . top)
                                    (slot . 0)))))
           (list
            (top (rx bos "*Backtrace*" eos))))

         ;; Left side - Search results, shells, REPLs, debugger ancillary
         ;; buffers. Generally, things that define a temporary context change.

         (cl-labels ((left (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (dedicated . t)
                                    (side . left)
                                    (slot . 0)))))
           (list
            (left '(derived-mode . grep-mode) '(window-width 80))
            (left (rx bos "*Embark Export: ") '(window-width 80))
            (left (rx bos "*Debugger-record*" eos)
                  '(slot . 1)
                  '(window-height . 0.3))
            (left (rx bos "*ielm*" eos))
            (left (rx bos "*org-roam-search") '(window-width . 80))
            (left (rx bos "CAPTURE-") '(window-width . 80))))

         ;; Right side - documentation, reference buffers & command outputs.

         (cl-labels ((right (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (dedicated . t)
                                    (side . right)
                                    (slot . 0)))))
           (list
            (right (mode-active-p 'gptel-mode) '(window-width . 80))
            (right (rx bos "*org-roam*" eos) '(window-width . 80))
            (right (rx bos "*org-roam-links*" eos) '(window-width . 80))
            (right (rx bos "*help*" eos) '(window-width . 80))
            (right (rx bos "*Man ") '(window-width . 80))
            (right (rx bos "*WoMan ") '(window-width . 80))
            (right (rx bos "*shell command output*" eos))
            (right (rx bos "*Org babel results*" eos))
            (right (rx bos "*async shell command*" eos))))


         ;; Bottom - Prompts, warnings, errors, compilation buffers.

         (cl-labels ((bottom (pred &rest overrides)
                       (cons pred `((display-buffer-reuse-window display-buffer-in-side-window)
                                    ,@overrides
                                    (side . bottom)
                                    (dedicated . t)
                                    (slot . 0)))))
           (list
            (bottom (rx bos "*eshell*" eos))
            (bottom (rx bos "*eldoc*" eos))
            (bottom (rx bos "*calendar*" eos))
            (bottom (rx bos " *Agenda Commands*" eos))
            (bottom (rx bos "*Org Select*" eos))
            (bottom (rx bos "*Org Note*" eos))
            (bottom (rx bos "*Org-Babel Error Output*" eos))
            (bottom (rx bos "*compilation*" eos))))

         ;; Buffers that should never pop up.

         (cl-labels ((suppress (pred &rest overrides)
                       (cons pred
                             `((display-buffer-no-window)
                               ,@overrides
                               (allow-no-window . t)))))
           (list
            (suppress (rx bos "*warnings*" eos))
            (suppress (rx bos "*async-native-compile-Log*" eos)))))))


;; Then, customise what display-buffer will do for all buffers not matching the
;; above rules.

;; In particular, prevent display-buffer from ever popping open another frame.

(setq display-buffer-fallback-action
      `((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-window
         display-buffer-in-previous-window
         display-buffer-use-some-window
         ,(defun +display-buffer-fallback (buffer &rest _)
            (when-let* ((win (split-window-sensibly)))
              (with-selected-window win
                (switch-to-buffer buffer)
                (help-window-setup (selected-window))))
            t))))

;; Apply a few more editor settings that are tightly coupled to display-buffer.

(setq window-combination-resize t)
(setq switch-to-buffer-in-dedicated-window 'pop)


;;; Load site files

(when (file-directory-p +site-files-directory)
  (dolist (file (directory-files +site-files-directory))
    (when (file-regular-p file)
      (load file t nil nil t))))



;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
