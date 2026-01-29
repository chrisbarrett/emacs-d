;;; treesit/init.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-sitter integration and expreg region expansion.

;;; Code:

(require '+autoloads)

(require 'treesit)

(setopt treesit-enabled-modes t)
(setopt treesit-auto-install-grammar 'always)

;; Keybindings for expreg
;; +/- to mark syntactic elements with tree-sitter.
;; - calls avy when no selection active.
(with-eval-after-load 'general
  (general-def
    :states '(normal motion)
    "-" (general-predicate-dispatch #'avy-goto-char-timer
          (region-active-p) #'expreg-contract)
    "+" #'+expreg-expand-dwim))



;;; init.el ends here
