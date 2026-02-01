;;; org-roam-init.el --- org-roam module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; org-roam provides workflows for working with documents for atomic notes (e.g. a
;; Zettelkasten); implements backlinks between documents for discovering
;; connected notes.

;;; Code:

(require '+autoloads)

(require '+corelib)
(require '+use-package-keywords)


;;; org-roam core configuration

(use-package org-roam
  :defer-incrementally
  ansi-color dash f rx seq magit-section emacsql

  :commands
  org-roam-buffer-toggle

  :init
  (+local-leader-set-key 'org-mode-map "TAB" #'org-roam-buffer-toggle)
  :general
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'org-roam-node-insert)

  :config
  (org-roam-db-autosync-mode +1)

  :config
  (+local-leader-set-key 'org-mode-map
    "l" '(nil :wk "aliases")
    "la" #'org-roam-alias-add
    "lx" #'org-roam-alias-remove)

  :custom
  (org-roam-extract-new-file-path "notes/${slug}.org")
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t) (org-roam-reflinks-section)))
  (org-roam-list-files-commands '(fd fdfind rg find))

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))

  :config
  (font-lock-add-keywords 'org-mode
                          `((,(rx bol "LINKS:") 0 '(face org-special-keyword) prepend)))

  (setq-hook! 'org-roam-find-file-hook
    org-id-link-to-org-use-id 'create-if-interactive))


;;; Node display performance optimization

(use-package org-roam
  :custom
  (org-roam-node-display-template #'+org-roam-node-formatted-olp)
  (org-roam-review-title-formatter #'+org-roam-node-formatted-olp))


;;; Marginalia integration

(use-package marginalia
  :after org-roam
  :config
  (alist-set! marginalia-annotators 'org-roam-node '(+org-roam-node-tags-annotator builtin none)))


;;; Roam buffer (backlinks) customizations

(use-package org-roam-mode
  :config
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)

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


;;; Nursery extensions

(use-package org-roam-review
  :commands (org-roam-review org-roam-review-list-recently-added)
  :hook (org-roam-review-mode . toggle-truncate-lines)
  :general-config
  (:states '(normal) :keymaps 'org-roam-review-mode-map
           "/"   #'org-roam-review-modify-tags
           "g r" #'org-roam-review-refresh))

(use-package org-roam-search
  :commands (org-roam-search))

(use-package org-roam-links
  :commands (org-roam-links))

(use-package org-roam-refill-previews
  :after org-roam
  :demand t
  :config
  (add-hook 'org-roam-preview-postprocess-functions #'org-roam-refill-previews))

(use-package org-roam-lazy-previews
  :after org-roam
  :demand t)

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
    "rR" #'org-roam-slipbox-refile))



;;; init.el ends here
