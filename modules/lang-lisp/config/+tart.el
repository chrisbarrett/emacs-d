;;; +tart.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+autoloads)
(require '+corelib)
(require 'evil)

(setq-hook! 'tart-signature-mode-hook
  evil-lookup-func #'+emacs-lisp-lookup-func)

;;; +tart.el ends here
