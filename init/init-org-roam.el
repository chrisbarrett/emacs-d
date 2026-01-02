;;; init-org-roam.el --- Configuration for org-roam -*- lexical-binding: t; -*-

;;; Commentary:

;; org-roam provides workflows for working with documents for atomic notes (e.g. a
;; Zettelkasten); implements backlinks between documents for discovering
;; connected notes.

;;; Code:

(require '+corelib)


(use-package org-roam :ensure t
  :after org

  :defer-incrementally
  ansi-color dash f rx seq magit-section emacsql

  ;; Autoload entrypoints

  :commands (org-roam-buffer-toggle)
  :init
  (+local-leader-set-key 'org-mode-map "TAB" #'org-roam-buffer-toggle)
  :general
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'org-roam-node-insert)

  ;; Keep DB in sync automatically.

  :config
  (org-roam-db-autosync-mode +1)

  ;; Set org-mode leader keys.
  :config
  (use-package org
    :config
    (+local-leader-set-key 'org-mode-map
      "l" '(nil :wk "aliases")
      "la" #'org-roam-alias-add
      "lx" #'org-roam-alias-remove))

  :custom
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))
  (org-roam-extract-new-file-path "notes/${slug}.org")
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t) (org-roam-reflinks-section)))
  (org-roam-list-files-commands '(fd fdfind rg find)) ; Prefer faster utilities

  ;; I conventionally use lines starting with `LINKS:' to create backlinks.
  ;; Highlight these.

  :config
  (font-lock-add-keywords 'org-mode
                          `((,(rx bol "LINKS:") 0 '(face org-special-keyword) prepend)))


  ;; Create IDs automatically when storing links into org buffers.

  (setq-hook! 'org-roam-find-file-hook
    org-id-link-to-org-use-id 'create-if-interactive)


  ;; Customise displayed node format. Using a function instead of a template
  ;; string improves performance of the completions list.

  :config
  (eval-and-compile
    (defun +org-roam-node-display-format (node)
      ;; FIXME
      (concat
       "${formatted-olp:*} "
       (propertize "@${slipbox:9}" 'face 'org-tag)
       "${tags:*}")))

  :custom
  (org-roam-node-display-template #'+org-roam-node-display-format))


;;; org-roam buffer customisations

;; NOTE: `org-roam-mode' is the major-mode of the backlinks buffer, *not* the
;; mode of notes files!

(use-package org-roam-mode
  :config
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  :custom
  (org-roam-node-formatter #'+org-roam-node-title-or-olp)

  ;; KLUDGE: Work around clashes with evil bindings.

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

(use-package org-roam-review
  :custom
  (org-roam-review-title-formatter #'org-roam-node-formatted-olp))


(provide 'init-org-roam)

;;; init-org-roam.el ends here
