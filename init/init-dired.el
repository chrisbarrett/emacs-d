;;; init-dired.el --- Directory browser -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Emacs' built-in file management interface.
(use-package dired
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
    "d" '(dired-hide-details-mode :wk "toggle details")
    "l" '(nil :wk "fs links")
    "ls" '(dired-do-symlink :wk "symlink (absolute)")
    "lr" '(dired-do-relsymlink :wk "symlink (relative)")
    "lh" '(dired-do-hardlink :wk "hardlink")
    "s" '(nil :wk "subdir")
    "si" '(dired-insert-subdir :wk "insert")
    "sx" '(dired-kill-subdir :wk "kill"))
  (setq-hook! 'dired-mode-hook
    dired-listing-switches (if (file-remote-p default-directory)
                               "-al"
                             "--almost-all --human-readable --group-directories-first --no-group")))


;; Extra functionality around omitting files, etc.
(use-package dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :custom
  (dired-omit-files (rx bos (or "." "__pycache__" "node_modules")))
  :init
  (+local-leader-set-key 'dired-mode-map
    "h" '(dired-omit-mode :wk "toggle hidden files")))


;; Icon set used by various packages.
(use-package nerd-icons :ensure t
  :autoload nerd-icons-codicon nerd-icons-faicon)


;; Show icons in dired.
(use-package nerd-icons-dired :ensure t
  :hook dired-mode-hook)


;; Makes dired buffers directly editable; the changes are interpreted into
;; operations on the corresponding file names.
(use-package wdired
  :general
  (:keymaps 'dired-mode-map "C-c C-e" #'wdired-change-to-wdired-mode))


;; Add extra font-lock to dired file listings.
(use-package diredfl :ensure t
  :hook (dired-mode-hook))


(provide 'init-dired)

;;; init-dired.el ends here
