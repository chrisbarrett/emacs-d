;;; init-elpaca.el --- Boot & configure elpaca package manager -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(defvar elpaca-repos-directory nil)

(defconst +chrisbarrett-elpaca-repos
  (seq-map (lambda (repo)
             (file-name-concat elpaca-repos-directory (concat repo "/")))
           '("emacs-beads" "nursery")))

(dolist (repo +chrisbarrett-elpaca-repos)
  (add-to-list 'trusted-content (abbreviate-file-name repo)))

;; Configure aspects of elpaca not required for initial package bootstrap.

(use-package elpaca
  :general-config
  (:states 'normal :keymaps 'elpaca-manager-mode-map "/" 'elpaca-ui-search)
  (:keymaps 'elpaca-info-mode-map "q" 'quit-window)

  :config
  (+dirlocals-set (list elpaca-repos-directory
                        elpaca-builds-directory)
    '((nil . ((mode . read-only))))))

(provide 'init-elpaca)

;;; init-elpaca.el ends here
