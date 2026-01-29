;;; early-init.el --- early Emacs init file -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:

;; This file is loaded by the editor early in the bootstrap process, before
;; resources like GUI frames are initialised. It is generally used to tune
;; critical aspects of the presentation or behaviour of the editor.

;;; Code:

(require 'cl-lib)
(cl-eval-when (compile)
  (require 'modus-themes nil t)
  (require 'use-package nil t))

;; Ensure Emacs doesn't block the session manager with subprocess exit prompts.
(setq confirm-kill-processes nil)



(require '+core-paths (file-name-concat user-emacs-directory "lisp" "+core-paths.el"))
(add-to-list 'load-path +init-dir)
(add-to-list 'load-path +lisp-dir)
(add-to-list 'load-path +config-dir)

;; Add module directories to load-path so module files (e.g. theme-lib) are
;; available before the full module system initializes in init.el.
(defun +module-directory-p (dir)
  "Return non-nil if DIR contains module system files."
  (and (file-directory-p dir)
       (cl-some (lambda (file)
                  (file-exists-p (file-name-concat dir file)))
                '("init.el" "lib.el" "packages.eld"))))

(when (file-directory-p +modules-directory)
  (dolist (dir (directory-files +modules-directory t "\\`[^.]"))
    (when (+module-directory-p dir)
      (add-to-list 'load-path dir))))

(setq package-enable-at-startup nil)



(setq load-prefer-newer t)

;;; Configure use-package

(setq use-package-verbose init-file-debug)
(setq use-package-always-defer t)
(setq use-package-enable-imenu-support t)
(setq use-package-minimum-reported-time 0.01)
(setq use-package-hook-name-suffix nil)
(setq use-package-compute-statistics t)

(unless init-file-debug
  (add-hook 'after-init-hook
            (lambda ()
              ;; Enable this when the configuration reaches a steady state; it
              ;; will make it much easier to read the macro-expanded output of
              ;; use-package calls.
              (setq use-package-expand-minimally t))))

(require '+use-package-keywords)
(+use-package-keywords-setup)


;;; Customise UI early in init sequence.

;; Configure theme early to ensure we don't observe the change during the
;; startup process.

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs nil)

(require 'theme-lib)
(setq +theme-light 'modus-operandi-tinted)
(setq +theme-dark 'modus-vivendi)

(set-face-attribute 'default nil :family "Fira Code")
(set-face-attribute 'variable-pitch nil :family "Fira Sans")

;; Defer theme loading until after a frame exists
(add-hook 'after-init-hook #'+theme-update)


;; Make the window-borders appear as padding instead. Not sure if this is really
;; usable yet, but it sure looks pretty.

(defun +sync-frame-parameters (&optional in-early-init)
  (modify-all-frames-parameters `((right-divider-width . 10)
                                  (internal-border-width . 10)
                                  ,@(when (equal system-type 'darwin)
                                      '((undecorated . t)))))

  ;; Themes aren't initialised until after early-init, so we can't access the
  ;; background colour yet.
  (unless in-early-init
    (let ((bg (face-attribute 'default :background)))
      (dolist (face '(fringe
                      window-divider
                      window-divider-first-pixel
                      window-divider-last-pixel))
        (face-spec-reset-face face)
        (set-face-foreground face bg)))))

(+sync-frame-parameters t)
(add-hook '+theme-changed-hook #'+sync-frame-parameters)
;; The minibuffer doesn't pick up the fringe parameters unless we sync again.
(add-hook 'after-init-hook #'+sync-frame-parameters)

(add-to-list 'initial-frame-alist '(name . "Emacs"))
(add-to-list 'initial-frame-alist '(initial . t))

;; Disable unneeded UI clutter

;; Take a cue from Doom's playbook and avoid calling the functions which can
;; trigger window-system redraws; instead, modify the frame parameters directly.

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (menu-bar-mode -1))))


;;; Customise native compilation

;; Silence unactionable warnings.

(let ((ncomp-warn-level (if init-file-debug
                            t
                          'silent)))
  (setq native-comp-async-report-warnings-errors ncomp-warn-level)
  (setq native-comp-warning-on-missing-source ncomp-warn-level))

;; Suppress obsolete macro warnings (when-let → when-let*) from third-party packages.
(unless init-file-debug
  (setq byte-compile-warnings '(not obsolete)))



;; Always prompt for "y" or "n", rather than "yes" or "no".
(setq use-short-answers t)

;; For safety, don't treat space as a "y".
(define-key y-or-n-p-map (kbd "SPC") nil)

(setq inhibit-x-resources t)
(setq inhibit-startup-screen t)

(set-language-environment "UTF-8")

;; Banish the customisation interface to the shadow realm.
(setq custom-file (make-temp-file "emacs-custom-"))


;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
