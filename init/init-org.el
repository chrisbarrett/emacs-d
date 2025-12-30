;;; init-org.el --- Bootstrap org-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)


;; org is a chonker; decompose the load process into smaller, incrementally
;; loaded features so it's less noticeable.
(+load-packages-incrementally '(calendar find-func format-spec org-macs org-compat org-faces org-entities
                                org-list org-pcomplete org-src org-footnote org-macro ob org org-modern
                                org-habit org-agenda org-capture))


;; org-mode - the reason why I can probably never switch to another editor.
(use-package org :ensure t ; NB. installed from org package archive.

  :hook ((org-mode-hook . abbrev-mode)
         (org-mode-hook . auto-fill-mode))

  :general ("C-c a" #'org-agenda)

  :custom
  (abbrev-file-name (file-name-concat org-directory "abbrev.el"))
  (org-babel-load-languages '((emacs-lisp . t)
                              (C . t)
                              (calc . t)
                              (shell . t)))
  :config
  (use-package mod-org :demand t)
  (use-package mod-org-link :after ol :demand t)
  (use-package mod-org-capture :after org-capture :demand t)
  (use-package mod-org-agenda :after org-agenda :demand t))


;; Provides extra evil keybindings for org-mode, org-agenda etc.
(use-package evil-org :ensure t
  :hook (org-mode-hook . evil-org-mode)
  :custom
  (evil-org-key-theme '(todo navigation insert textobjects additional calendar))
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :demand t
    :config
    (evil-org-agenda-set-keys)
    (evil-define-key 'motion org-agenda-mode-map
      (kbd "v") #'org-agenda-view-mode-dispatch
      (kbd "SPC") nil
      (kbd "/") #'org-agenda-filter)))


;; Group items in the agenda
(use-package org-super-agenda :ensure t
  :after org-agenda
  :demand t
  :custom
  (org-super-agenda-hide-empty-groups t)
  :config
  (org-super-agenda-mode +1)
  ;; Clear the keymap to ensure the regular evil keybindings for org-agenda
  ;; work.
  (setq org-super-agenda-header-map (make-sparse-keymap)))


;; Provides workflows for working with documents for atomic notes (e.g. a
;; Zettelkasten); implements backlinks between documents for discovering
;; connected notes.
(use-package org-roam :ensure t
  :after org
  :config
  (use-package mod-org-roam :demand t)

  :defer-incrementally
  ansi-color dash f rx seq magit-section emacsql

  ;; Autoload entrypoints
  :commands (org-roam-buffer-toggle)
  :init
  (+local-leader-set-key 'org-mode-map "TAB" #'org-roam-buffer-toggle)
  :general
  (:states '(motion insert normal) :keymaps 'org-mode-map
           "C-c C-i" #'org-roam-node-insert))


;; Easily pop open comments or strings for editing in a dedicated buffer.
(use-package separedit :ensure t
  :commands (separedit-commit))


;; Provides visual enhancements that make org-mode look less cluttered and
;; more in-line with modern UX ideas.
(use-package org-modern :ensure t
  :after org
  :demand t
  :custom
  (org-modern-hide-stars nil)
  (org-modern-fold-stars
   '(("▶" . "▼") ("▹" . "▿") ("▸" . "▾") ("⯈" . "⯆")))
  (org-modern-block-name
   `(("src" . ("" "◌"))
     ("quote" . ("" "◌"))
     ("example" . ("" "◌"))))
  :config
  (global-org-modern-mode +1)
  (custom-theme-set-faces 'user
                          '(org-todo ((t (:bold t :inverse-video t))))
                          ;; Based on `eldoc-highlight-function-argument'
                          '(org-modern-date-active ((((background dark))
                                                     (:foreground "#dfaf7a"
                                                      :background "#381d0f"
                                                      :weight light
                                                      :inherit org-modern-label))
                                                    (((background light))
                                                     (:foreground "#884900"
                                                      :background "#f8f0d0"
                                                      :weight light
                                                      :inherit org-modern-label)))))

  (let ((custom-todos
         '(("WAIT" warning org-todo)
           ("PROJECT" font-lock-keyword-face org-todo))))

    (setq org-modern-todo-faces custom-todos)
    (setq org-todo-keyword-faces
          (seq-map (apply-partially #'take 2) custom-todos))))


;; Create org-mode links from URLs on the clipboard.
(use-package org-cliplink :ensure t
  :general (:keymaps 'org-mode-map "C-c l" #'org-cliplink))


;; The calendar widget used in org-mode.
(use-package calendar
  :custom
  (calendar-mode-line-format nil)
  (calendar-date-style 'iso)
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1)
  ;; :config
  ;; (setf (car calendar-time-display-form) '24-hours)
  )


;; Exporter backend for github-flavoured markdown.
(use-package ox-gfm :ensure t
  :after ox
  :demand t)


(provide 'init-org)

;;; init-org.el ends here
