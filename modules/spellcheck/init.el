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

;; spell-fu hooks - enable in text, prog, and conf modes
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
           "zx" #'spell-fu-word-remove))

(use-package flyspell-correct
  :general
  (:states 'normal "z SPC" #'flyspell-correct-at-point))


(add-hook 'spell-fu-mode-hook #'+spellcheck-add-dictionaries)
(add-hook 'org-mode-hook #'+spellcheck-setup-org)

;;; init.el ends here
