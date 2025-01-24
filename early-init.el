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

(eval-and-compile
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp")))

;; Configure theme early to ensure we don't observe the change during the
;; startup process.

(setq +theme-light 'modus-operandi)
(setq +theme-dark 'modus-vivendi)
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs nil)
(require '+theme)
(+theme-update)

;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
