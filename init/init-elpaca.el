;;; init-elpaca.el --- Boot & configure elpaca package manager -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require '+load-incrementally)

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

;; Configure aspects of elpaca not required for initial package bootstrap.
;;

(with-eval-after-load 'general
  ;; KLUDGE: `:general-config' isn't available until after `general' has been
  ;; loaded, which is later in the init sequence. Use a quote+eval to suppress
  ;; macro-expansion of this form.
  (eval
   '(use-package elpaca
      :general-config
      (:states 'normal :keymaps 'elpaca-manager-mode-map "/" #'elpaca-ui-search)
      (:keymaps 'elpaca-info-mode-map "q" #'quit-window))))

(provide 'init-elpaca)

;;; init-elpaca.el ends here
