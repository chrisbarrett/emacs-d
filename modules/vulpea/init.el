;;; vulpea-init.el --- Vulpea module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Vulpea provides a database layer for org-mode notes with fast queries,
;; backlink discovery, and a sidebar widget framework.

;;; Code:

(require '+autoloads)

(require '+corelib)
(require '+use-package-keywords)


;;; Core vulpea configuration

(use-package vulpea
  :defer-incrementally
  dash s seq emacsql

  :commands
  vulpea-find
  vulpea-insert

  :init
  (+local-leader-set-key 'org-mode-map "TAB" #'vulpea-ui-sidebar-toggle)
  :general
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'vulpea-insert)

  :custom
  (vulpea-select-describe-fn #'+vulpea-note-describe-olp)
  (vulpea-create-default-template '(:file-name "notes/${slug}.org"))
  (vulpea-buffer-alias-property "ROAM_ALIASES")

  :config
  (setq vulpea-db-location (no-littering-expand-var-file-name "vulpea/vulpea.db"))
  (vulpea-db-autosync-mode +1)

  (+local-leader-set-key 'org-mode-map
    "l" '(nil :wk "aliases")
    "la" #'vulpea-buffer-alias-add
    "lx" #'vulpea-buffer-alias-remove)

  (font-lock-add-keywords 'org-mode
                          `((,(rx bol "LINKS:") 0 '(face org-special-keyword) prepend)))

  (add-hook 'find-file-hook
            (defun +vulpea-set-id-link-policy-h ()
              (when (and buffer-file-name
                         (file-in-directory-p buffer-file-name +notes-directory))
                (setq-local org-id-link-to-org-use-id 'create-if-interactive)))))



;;; Sidebar (vulpea-ui)

(use-package vulpea-ui
  :after vulpea
  :demand t
  :custom
  (vulpea-ui-sidebar-position 'right)
  (vulpea-ui-sidebar-size 0.33)
  (vulpea-ui-backlinks-show-preview t)
  :config
  (add-hook 'vulpea-ui-sidebar-mode-hook #'turn-on-visual-line-mode))


;;; init.el ends here
