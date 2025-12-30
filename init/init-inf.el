;;; init-inf.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package comint
  ;; Emacs' basic system for hosting interactive command interpreters.
  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048) ; double the default.
  )

(use-package compile
  ;; Integration for running compilers and other processes from inside Emacs.
  :config
  (use-package mod-compilation :demand t))


(provide 'init-inf)

;;; init-inf.el ends here
