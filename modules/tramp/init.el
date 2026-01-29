;;; init.el --- TRAMP remote access -*- lexical-binding: t; -*-

;;; Commentary:

;; Configure TRAMP for remote file access.

;;; Code:

(require '+autoloads)

(require '+corelib)

(use-package tramp
  :config
  (pushnew! tramp-remote-path 'tramp-own-remote-path))



;;; init.el ends here
