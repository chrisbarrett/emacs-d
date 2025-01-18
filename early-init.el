;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Disable unneeded UI clutter.

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(when (equal system-type 'darwin)
  (modify-all-frames-parameters '((undecorated . t))))
