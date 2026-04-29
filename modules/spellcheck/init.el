;;; init.el --- Spellcheck module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures ispell, spell-fu, and flyspell-correct for spell checking.
;; Adds vim-style keybindings (zn, zp, zg, zx, z SPC).

;;; Code:

(require '+autoloads)


;; Built-in ispell configuration
(use-package ispell
  :custom
  (ispell-dictionary "en_AU")
  (ispell-personal-dictionary (file-name-concat org-directory "aspell.en.pws"))
  :functions ispell-set-spellchecker-params
  :config
  (require 'exec-path-from-shell)
  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))

  (ispell-set-spellchecker-params)

  ;; Treat aspell dictionaries as UTF-8
  (add-to-list 'file-coding-system-alist
               (cons (rx "aspell." (+? nonl) ".pws" eos) 'utf-8-unix)))

(use-package spell-fu
  :hook
  (text-mode-hook . spell-fu-mode)
  (prog-mode-hook . spell-fu-mode)
  (conf-mode-hook . spell-fu-mode)
  :general
  (:states '(normal motion)
           "zn" #'spell-fu-goto-next-error
           "zp" #'spell-fu-goto-previous-error
           "zg" #'spell-fu-word-add
           "zx" #'spell-fu-word-remove)
  :config
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_AU"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "fr"))

  (setq-hook! markdown-mode
    spell-fu-faces-exclude '(markdown-pre-face markdown-inline-code-face
                             markdown-reference-face))

  (setq-hook! org-mode
    spell-fu-faces-exclude '(org-meta-line org-link org-code org-block
                             org-block-begin-line org-block-end-line
                             org-footnote org-tag org-modern-tag org-verbatim)))

(use-package flyspell-correct
  :general
  (:states 'normal "z SPC" #'flyspell-correct-at-point))

;;; init.el ends here
