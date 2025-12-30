;;; init-nolitter.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package no-littering :ensure (:wait t) :demand t
  :config
  (no-littering-theme-backups))

(provide 'init-nolitter)

;;; init-nolitter.el ends here
