;;; init-spellcheck.el --- Spellchecking -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Built-in spellchecker. I don't actually use it directly, but other packages
;; reference its configuration.
(use-package ispell
  :custom
  (ispell-dictionary "en_AU")
  (ispell-personal-dictionary (file-name-concat org-directory "aspell.en.pws"))

  :functions (ispell-set-spellchecker-params)

  :config
  ;; Ensure exec-path has been set up before we attempt to look for aspell.
  (require 'exec-path-from-shell nil t)
  (unless (executable-find "aspell")
    (warn "Could not find aspell program; spell checking will not work"))

  (ispell-set-spellchecker-params)

  ;; Treat aspell dictionaries as UTF-8. Note that aspell itself needs the
  ;; `utf-8' token in the header line for a dictionary file to use UTF-8
  ;; encoding.
  (add-to-list 'file-coding-system-alist
               (cons (rx "aspell." (+? nonl) ".pws" eos) 'utf-8-unix)))

;; A more lightweight spell-checker than the built-in.
(use-package spell-fu :ensure t
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general
  (:states '(normal motion)
           "zn" #'spell-fu-goto-next-error
           "zp" #'spell-fu-goto-previous-error
           "zg" #'spell-fu-word-add
           "zx" #'spell-fu-word-remove)

  :config
  (add-hook! 'spell-fu-mode-hook
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_AU"))
    (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "fr")))

  (setq-hook! 'org-mode-hook
    spell-fu-faces-exclude '(org-meta-line org-link org-code org-block
                             org-block-begin-line org-block-end-line
                             org-footnote org-tag org-modern-tag org-verbatim)))

;; Provides a nicer command for working with spelling corrections.
(use-package flyspell-correct :ensure t
  :after spell-fu
  :general
  (:states 'normal "z SPC" #'flyspell-correct-at-point))

(provide 'init-spellcheck)

;;; init-spellcheck.el ends here
