;;; init.el --- Format module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures code formatting, whitespace management, and indentation behavior.
;; Apheleia provides format-on-save. Whitespace trimming is aggressive by default.

;;; Code:

(require '+autoloads)

;; Align: mode-specific alignment (C-x a a for align-regexp).
(with-eval-after-load 'general
  (general-def "C-x a a" #'align-regexp))

;; Indentation: TAB completion after partial word/paren/punct.
(setq tab-first-completion 'word-or-paren-or-punct)

;; Apheleia: format-on-save framework.
;; Loaded lazily on first file open, then enables global mode.
(with-eval-after-load 'apheleia
  (setq apheleia-remote-algorithm 'local)
  (setq apheleia-formatters-respect-fill-column t)
  (apheleia-global-mode +1))

;; Trigger apheleia load on first file open.
(when (boundp '+first-file-hook)
  (add-hook '+first-file-hook
            (lambda ()
              (require 'apheleia nil t))))

;; Whitespace trimming: aggressive by default.
(defvar-local +trim-trailing-whitespace-aggressively t
  "When non-nil, delete all trailing whitespace on save.")

(add-hook 'before-save-hook
          (lambda ()
            (when +trim-trailing-whitespace-aggressively
              (delete-trailing-whitespace))))

;; Tabify: only convert spaces to tabs at line beginning.
(setq tabify-regexp "^\t* [ \t]+")



;;; init.el ends here
