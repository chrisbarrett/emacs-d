;;; init-elpaca.el --- Boot & configure elpaca package manager -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require '+load-incrementally)

(defvar elpaca-repos-directory nil)

(defconst +chrisbarrett-elpaca-repos
  (seq-map (lambda (repo)
             (file-name-concat elpaca-repos-directory (concat repo "/")))
           '("emacs-beads" "nursery")))

(dolist (repo +chrisbarrett-elpaca-repos)
  (add-to-list 'trusted-content (abbreviate-file-name repo)))

;; Make sure I don't accidentally start loading super-expensive packages on startup.

(defconst +expensive-packages '(org org-roam org-agenda forge))

(add-transient-hook! 'after-init-hook
  (when-let* ((loaded (seq-filter #'featurep +expensive-packages)))
    (warn "The following package(s) were loaded eagerly, rather than deferred: %S" loaded)))

(add-hook 'elpaca-after-init-hook #'+load-packages-incrementally-h)

;; Configure aspects of elpaca not required for initial package bootstrap.

(use-package elpaca
  :general-config
  (:states 'normal :keymaps 'elpaca-manager-mode-map "/" 'elpaca-ui-search)
  (:keymaps 'elpaca-info-mode-map "q" 'quit-window))

(provide 'init-elpaca)

;;; init-elpaca.el ends here
