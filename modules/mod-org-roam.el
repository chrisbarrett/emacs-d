;;; mod-org-roam.el --- org-roam configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'org-roam)
(require '+org-roam)
(require 'general)

(+local-leader-set-key 'org-mode-map
  "l" '(nil :wk "aliases")
  "la" #'org-roam-alias-add
  "lx" #'org-roam-alias-remove)


;;; org-mode integration

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)))

;; Customise completion UI
(setq org-roam-node-display-template
      (concat
       "${formatted-olp:*} "
       (propertize "@${slipbox:9}" 'face 'org-tag)
       "${tags:*}"))

;; prefer faster utilities
(setq org-roam-list-files-commands '(fd fdfind rg find))

(setq org-roam-extract-new-file-path "notes/${slug}.org")

(setq org-roam-mode-sections '((org-roam-backlinks-section :unique t)
                               (org-roam-reflinks-section)))

(setq-hook! 'org-roam-find-file-hook
  org-id-link-to-org-use-id 'create-if-interactive)

;; I use lines starting with `LINKS:' to create backlinks. Highlight these.
(font-lock-add-keywords 'org-mode
                        `((,(rx bol "LINKS:") 0 '(face org-special-keyword) prepend)))



;;; org-roam buffer customisations

;; NOTE: `org-roam-mode' is the major-mode of the backlinks buffer, *not* the
;; mode of notes files!

(add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)

(require '+org-roam)
(setq org-roam-node-formatter #'+org-roam-node-title-or-olp)
(setq org-roam-review-title-formatter #'org-roam-node-formatted-olp)

;; Work around clashes with evil bindings.

(add-hook! 'org-roam-mode-hook (set-keymap-parent org-roam-mode-map nil))

(general-def :keymaps 'org-roam-mode-map
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
  "C-k"     #'magit-section-backward)



(org-roam-db-autosync-mode +1)

(provide 'mod-org-roam)

;;; mod-org-roam.el ends here
