;;; init.el --- Spellcheck module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures ispell, spell-fu, and flyspell-correct for spell checking.
;; Adds vim-style keybindings (zn, zp, zg, zx, z SPC).

;;; Code:

(require '+autoloads)

(require 'exec-path-from-shell nil t)

;; Built-in ispell configuration
(with-eval-after-load 'ispell
  (setq ispell-dictionary "en_AU")
  (setq ispell-personal-dictionary
        (file-name-concat org-directory "aspell.en.pws"))

  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))

  (ispell-set-spellchecker-params)

  ;; Treat aspell dictionaries as UTF-8
  (add-to-list 'file-coding-system-alist
               (cons (rx "aspell." (+? nonl) ".pws" eos) 'utf-8-unix)))

;; spell-fu hooks - enable in text, prog, and conf modes
(with-eval-after-load 'spell-fu
  (add-hook 'spell-fu-mode-hook #'+spellcheck-add-dictionaries)
  (add-hook 'org-mode-hook #'+spellcheck-setup-org))

(add-hook 'text-mode-hook #'spell-fu-mode)
(add-hook 'prog-mode-hook #'spell-fu-mode)
(add-hook 'conf-mode-hook #'spell-fu-mode)

;; Vim-style keybindings
(with-eval-after-load 'evil
  (with-eval-after-load 'spell-fu
    (evil-define-key* '(normal motion) 'global
      "zn" #'spell-fu-goto-next-error
      "zp" #'spell-fu-goto-previous-error
      "zg" #'spell-fu-word-add
      "zx" #'spell-fu-word-remove))

  (with-eval-after-load 'flyspell-correct
    (evil-define-key* 'normal 'global
      (kbd "z SPC") #'flyspell-correct-at-point)))



;;; init.el ends here
