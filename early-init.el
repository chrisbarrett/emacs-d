;; -*- lexical-binding: t; -*-

(setq inhibit-startup-screen t)

(setq package-enable-at-startup nil)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)

(setq frame-resize-pixelwise t)

;; Always prompt for "y" or "n", rather than "yes" or "no".
(setq use-short-answers t)
;; For safety, don't treat space as a "y".
(define-key y-or-n-p-map (kbd "SPC") nil)

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
