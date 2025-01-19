;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq frame-resize-pixelwise t)
(setq use-short-answers t)
(setq inhibit-x-resources t)

;; Disable unneeded UI clutter.

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(when (equal system-type 'darwin)
  (modify-all-frames-parameters '((undecorated . t))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
