;;; early-init.el --- early Emacs init file -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is loaded by the editor early in the bootstrap process, before
;; resources like GUI frames are initialised. It is generally used to tune
;; critical aspects of the presentation or behaviour of the editor.

;;; Code:

(eval-and-compile
  (add-to-list 'load-path (file-name-concat user-emacs-directory "lisp")))

(setq package-enable-at-startup nil)



;;; Configure use-package

(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)

;; Enable this when the configuration reaches a steady state; it will make it
;; much easier to read the macro-expanded output of use-package calls.

;; (setq use-package-expand-minimally t)

(require '+load-incrementally)
(+load-incrementally-setup-use-package-keywords)


;;; Customise UI early in init sequence.

;; Disable unneeded UI clutter.
;;
;; Take a cue from Doom's playbook and avoid calling the functions which can
;; trigger window-system redraws; instead, modify the frame parameters directly.

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (equal system-type 'darwin)
  (modify-all-frames-parameters '((undecorated . t))))

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq frame-resize-pixelwise t)

;; Configure theme early to ensure we don't observe the change during the
;; startup process.

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs nil)

(require '+theme)
(setq +theme-light 'modus-operandi)
(setq +theme-dark 'modus-vivendi)

;; Sync the theme with the window system.
(+theme-update)



;; Always prompt for "y" or "n", rather than "yes" or "no".
(setq use-short-answers t)

;; For safety, don't treat space as a "y".
(define-key y-or-n-p-map (kbd "SPC") nil)

(setq inhibit-x-resources t)
(setq inhibit-startup-screen t)

(set-language-environment "UTF-8")


;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
