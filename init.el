;;; init.el --- main Emacs init file -*- lexical-binding: t; no-byte-compile: t; no-update-autoloads: t; no-native-compile: t; -*-

;;; Commentary:

;; This is the primary Emacs init file, loaded by the editor on startup. It is
;; loaded after early-init.el.

;;; Code:

(when (< emacs-major-version 30)
  (user-error "Emacs 30 required"))

(defvar org-directory "~/org/")
(defvar org-roam-directory "~/org/roam/")
(defvar org-default-notes-file "~/org/notes.org")

(defvar +site-files-directory (file-name-concat user-emacs-directory "site/"))

(add-to-list 'trusted-content (file-name-concat user-emacs-directory "early-init.el"))
(add-to-list 'trusted-content (file-name-concat user-emacs-directory "init.el"))
(add-to-list 'trusted-content +init-dir)
(add-to-list 'trusted-content +lisp-dir)
(add-to-list 'trusted-content +config-dir)
(add-to-list 'trusted-content +modules-directory)


;;; Bootstrap Elpaca & critical packages

;; Suppress warning when loading Elpaca with latest Emacs.
(add-to-list 'warning-suppress-types '(elpaca core \30.2))
(add-to-list 'warning-suppress-types '(elpaca core \31.0.50))

(unless (featurep 'elpaca)
  (load-file (file-name-concat user-emacs-directory "elpaca-bootstrap.el")))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; General provides a featureful key binding system. It makes defining leader
;; key bindings much easier, and must be loaded immediately for its use-package
;; integration.
(use-package general :ensure t :demand t)

;; Configure Emacs features & packages to follow a structured approach to
;; writing cache files, temp data, etc.
(use-package no-littering :ensure t :demand t
  :config
  (no-littering-theme-backups))

;; Ensure we never attempt to load outdated ELC files.
(use-package auto-compile :ensure t :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Use the shell to get some environment vars; necessary when the window
;; system runs Emacs under a very different process environment.
;;
;; Also, turns out we need this for direnv to work right in compilation buffers.
(use-package exec-path-from-shell :ensure t
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

;; Block until these packages are activated.
(elpaca-wait)


;;; Module System Integration

(require '+modules)

;; Install packages from modules early in the queue.
(+modules-install-packages (+modules-collect-packages))

;; Register autoloads so module functions are available before loading.
(+modules-register-autoloads (+modules-collect-autoloads))


;; Key init files that must be loaded early in the sequence.

(use-package init-elpaca :demand t)

(use-package init-hooks
  :demand t
  :init
  (defconst +expensive-packages '(org org-roam org-agenda forge))
  :config
  ;; Warn if expensive packages were loaded during init sequence.
  (add-transient-hook! 'after-init-hook
    (when-let* ((loaded (seq-filter #'featurep +expensive-packages)))
      (warn "The following package(s) were loaded eagerly, rather than deferred: %S" loaded))))

(use-package +use-package-keywords
  :demand t
  :hook (elpaca-after-init-hook . +load-packages-incrementally-h)
  :config
  (+load-packages-incrementally '(calendar find-func format-spec org-macs org-compat org-faces org-entities
                                  org-list org-pcomplete org-src org-footnote org-macro ob org org-modern
                                  org-habit org-agenda org-capture)))

;; Load init/**.el

(dolist (file (directory-files-recursively +init-dir (rx ".el" eos)))
  (let ((basename (file-name-base file)))
    (unless (string-match-p (rx bol (any ".~#_")) basename)
      (eval `(use-package ,(intern basename)
               :demand t)))))


;;; Module init files

;; Load module init.el files after all autoloads are registered.
(+modules-load-inits (+modules-collect-init-files))


;;; Load site/**.el

(when (file-directory-p +site-files-directory)
  (dolist (file (directory-files-recursively +site-files-directory (rx ".el" eos)))
    (let ((basename (file-name-base file)))
      (unless (string-match-p (rx bol (any ".~#_")) basename)
        (load file t nil nil t)))))


;;; No-op footer to silence byte-compiler warning.

;; (provide 'init)

;;; init.el ends here
