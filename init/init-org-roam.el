;;; init-org-roam.el --- Configuration for org-roam -*- lexical-binding: t; -*-

;;; Commentary:

;; org-roam provides workflows for working with documents for atomic notes (e.g. a
;; Zettelkasten); implements backlinks between documents for discovering
;; connected notes.

;;; Code:

(require '+corelib)

;; Define autoloads and incremental loading semantics.

(use-package org-roam
  :ensure t

  :defer-incrementally
  ansi-color dash f rx seq magit-section emacsql

  :commands
  org-roam-buffer-toggle)

(use-package org-roam-node
  :autoload
  org-roam-node-file-title
  org-roam-node-find
  org-roam-node-olp
  org-roam-node-tags
  org-roam-node-title)

(use-package org-roam-slipbox
  :autoload org-roam-node-slipbox)




(use-package org-roam
  :after org

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
  (org-roam-extract-new-file-path "notes/${slug}.org")
  (org-roam-mode-sections '((org-roam-backlinks-section :unique t) (org-roam-reflinks-section)))
  (org-roam-list-files-commands '(fd fdfind rg find)) ; Prefer faster utilities

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))

  ;; I conventionally use lines starting with `LINKS:' to create backlinks.
  ;; Highlight these.

  :config
  (font-lock-add-keywords 'org-mode
                          `((,(rx bol "LINKS:") 0 '(face org-special-keyword) prepend)))


  ;; Create IDs automatically when storing links into org buffers.

  (setq-hook! 'org-roam-find-file-hook
    org-id-link-to-org-use-id 'create-if-interactive))


;;; Helper functions used by configuration.

(defconst +org-roam-sensitive-tags '("daily" "sensitive" "private")
  "Tags that indicate a node should not be displayed without explicit user action.")

(defun +org-roam-node-sensitive-p (node)
  (null (seq-intersection (org-roam-node-tags node) +org-roam-sensitive-tags)))

(defun +org-roam-node-find (&optional include-sensitive)
  "Find an org-roam node. See `org-roam-node-find'.

With optional prefix arg INCLUDE-SENSITIVE, include nodes with tags in
`+org-roam-sensitive-tags'."
  (interactive "P")
  (org-roam-node-find nil
                      nil
                      (unless include-sensitive
                        #'+org-roam-node-sensitive-p)))


;;; Fix poor node completion performance

;; Use a function rather than a template string to generate org-roam node
;; completion candidates; this is *orders of magnitude* faster.

(use-package org-roam
  :custom
  (org-roam-node-display-template #'+org-roam-node-formatted-olp)
  (org-roam-node-formatter #'+org-roam-node-title-or-olp)
  (org-roam-review-title-formatter #'+org-roam-node-formatted-olp)

  :config
  (defun +org-roam-node-formatted-olp (node)
    "Construct a title string for NODE that includes its outline path.

This is useful for distinguishing or narrowing nodes according to a
topic without using tags."
    (pcase-let ((`(,title . ,rest)
                 (thread-last
                   (append (list (org-roam-node-file-title node))
                           (org-roam-node-olp node)
                           (list (org-roam-node-title node)))
                   (seq-filter #'stringp)
                   (seq-mapcat (lambda (it) (split-string it ":")))
                   (seq-map #'string-trim)
                   (seq-uniq)
                   (nreverse))))
      (let ((prefix (seq-map (lambda (it) (propertize it 'face 'org-property-value)) (nreverse rest)))
            (title (propertize title 'face 'org-roam-title)))

        (string-join (append prefix (list title))
                     (propertize ": " 'face 'org-property-value))))))

;; Show node tags in completing-read marginalia.

(use-package marginalia
  :after org-roam
  :config
  (defun +org-roam-node-tags-annotator (cand)
    (let* ((node (get-text-property 0 'node cand))
           (slipbox (org-roam-node-slipbox node))
           (all-tags (seq-filter #'identity
                                 (seq-keep (lambda (tag)
                                             (unless (equal tag slipbox)
                                               (concat "#" tag)))
                                           (org-roam-node-tags node)))))
      (marginalia--fields
       ((concat "@" slipbox) :face 'org-roam-dim)
       ((string-join all-tags " ") :face 'org-tag))))

  (alist-set! marginalia-annotators 'org-roam-node '(+org-roam-node-tags-annotator builtin none)))


;;; Roam buffer customisations

;; NOTE: `org-roam-mode' is the major-mode of the backlinks buffer, *not* the
;; mode of notes files!

(use-package org-roam-mode
  :config
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)

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


(provide 'init-org-roam)

;;; init-org-roam.el ends here
