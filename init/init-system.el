;;; init-system.el --- Host system integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

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


;; Adds direnv support.
(use-package envrc :ensure t
  :hook (+first-file-hook . envrc-global-mode)
  :custom
  (envrc-show-summary-in-minibuffer nil) ; very noisy.
  )


;; Adds support for mise - https://mise.jdx.dev.
(use-package mise :ensure t
  :hook (+first-file-hook . global-mise-mode)
  :if (executable-find "mise"))


;; Use existing Emacs instances to edit files as $EDITOR.
(use-package server
  :if (display-graphic-p)
  :demand t
  :functions server-running-p
  :config
  (unless (server-running-p)
    (server-start)))


;; Copy to and paste from GUI clipboard in terminal.
(use-package clipetty :ensure t
  :after-call +first-input-hook +first-file-hook
  :config (global-clipetty-mode +1))


(use-package xt-mouse
  :demand t
  :config
  (xterm-mouse-mode +1))


(use-package tramp
  :config
  (pushnew! tramp-remote-path 'tramp-own-remote-path))


;; Run a hook at midnight; cleans up old buffers by default. Useful for
;; preventing an Emacs server instance from drowning in open buffers
;; throughout the work week.
;;
;; The behaviour of the buffer cleanup can be customised via the
;; `clean-buffer-list-*' variables.
(use-package midnight
  :after-call +first-file-hook +first-buffer-hook
  :config
  (midnight-mode +1))


(provide 'init-system)

;;; init-system.el ends here
