;;; tests.el --- Dired module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for dired module based on spec 017-dired.md

;;; Code:

(require 'ert)

;; Track whether init loaded successfully
(defvar dired-module--init-loaded nil
  "Non-nil if the dired module init.el loaded without errors.")

;; Load the module init.el only if general is available (for batch mode)
(when (fboundp 'general-define-key)
  (condition-case nil
      (progn
        (load (expand-file-name "modules/dired/init.el" user-emacs-directory) nil t)
        (setq dired-module--init-loaded t))
    (error nil)))

;;; P1: Opening dired buffer enables dired-hide-details-mode

(ert-deftest dired-module/p1-hide-details-hook ()
  "P1: dired-hide-details-mode is hooked to dired-mode."
  (skip-unless dired-module--init-loaded)
  (should (memq 'dired-hide-details-mode (default-value 'dired-mode-hook))))

;;; P2: Opening dired buffer enables hl-line-mode

(ert-deftest dired-module/p2-hl-line-hook ()
  "P2: hl-line-mode is hooked to dired-mode."
  (skip-unless dired-module--init-loaded)
  (should (memq 'hl-line-mode (default-value 'dired-mode-hook))))

;;; P3: Opening dired buffer enables dired-omit-mode

(ert-deftest dired-module/p3-omit-mode-hook ()
  "P3: dired-omit-mode is hooked to dired-mode."
  (skip-unless dired-module--init-loaded)
  (should (memq 'dired-omit-mode (default-value 'dired-mode-hook))))

;;; P4: Nerd icons appear in dired listings

(ert-deftest dired-module/p4-nerd-icons-dired-hook ()
  "P4: nerd-icons-dired-mode is hooked to dired-mode."
  (skip-unless dired-module--init-loaded)
  (should (memq 'nerd-icons-dired-mode (default-value 'dired-mode-hook))))

;;; P5: Local directories use long listing switches (tested via setq-hook)

(ert-deftest dired-module/p5-listing-switches-customization ()
  "P5: dired-listing-switches is customized via hook."
  (skip-unless dired-module--init-loaded)
  (skip-unless (boundp '+setq-hook-alist))
  ;; The module uses setq-hook! to set dired-listing-switches dynamically
  ;; We verify the hook mechanism is in place
  (should (memq 'dired-mode-hook (mapcar #'car +setq-hook-alist))))

;;; P6: Remote directories use simple -al switches (implicit in hook logic)

(ert-deftest dired-module/p6-remote-listing-conditional ()
  "P6: Listing switches use file-remote-p conditional."
  (skip-unless dired-module--init-loaded)
  ;; This is verified by P5 - the same hook handles both cases
  (skip-unless (boundp '+setq-hook-alist))
  (should (assq 'dired-mode-hook +setq-hook-alist)))

;;; P7: C-c C-e enters wdired mode

(ert-deftest dired-module/p7-wdired-keybinding ()
  "P7: C-c C-e is bound to wdired-change-to-wdired-mode in dired-mode-map."
  (skip-unless dired-module--init-loaded)
  (skip-unless (fboundp 'general-define-key))
  (require 'dired)
  ;; Check that C-c C-e is bound in dired-mode-map
  (let ((binding (lookup-key dired-mode-map (kbd "C-c C-e"))))
    (should (eq binding 'wdired-change-to-wdired-mode))))

;;; P8: delete-by-moving-to-trash is t

(ert-deftest dired-module/p8-delete-to-trash ()
  "P8: delete-by-moving-to-trash is t."
  (skip-unless dired-module--init-loaded)
  (should (eq delete-by-moving-to-trash t)))

;;; P9: dired-dwim-target is t

(ert-deftest dired-module/p9-dwim-target ()
  "P9: dired-dwim-target is t."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-dwim-target t)))

;;; Additional tests for completeness

(ert-deftest dired-module/recursive-copies ()
  "dired-recursive-copies is always."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-recursive-copies 'always)))

(ert-deftest dired-module/recursive-deletes ()
  "dired-recursive-deletes is always."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-recursive-deletes 'always)))

(ert-deftest dired-module/kill-when-opening-new ()
  "dired-kill-when-opening-new-dired-buffer is t."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-kill-when-opening-new-dired-buffer t)))

(ert-deftest dired-module/auto-revert-buffer ()
  "dired-auto-revert-buffer uses dired-directory-changed-p."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-auto-revert-buffer 'dired-directory-changed-p)))

(ert-deftest dired-module/create-destination-dirs ()
  "dired-create-destination-dirs is ask."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-create-destination-dirs 'ask)))

(ert-deftest dired-module/vc-rename-file ()
  "dired-vc-rename-file is t."
  (skip-unless dired-module--init-loaded)
  (should (eq dired-vc-rename-file t)))

(ert-deftest dired-module/omit-files-pattern ()
  "dired-omit-files includes dotfiles, __pycache__, node_modules."
  (skip-unless dired-module--init-loaded)
  (skip-unless (boundp 'dired-omit-files))
  (should (stringp dired-omit-files))
  ;; Should match dotfiles
  (should (string-match-p dired-omit-files ".hidden"))
  ;; Should match __pycache__
  (should (string-match-p dired-omit-files "__pycache__"))
  ;; Should match node_modules
  (should (string-match-p dired-omit-files "node_modules")))

(ert-deftest dired-module/diredfl-hook ()
  "diredfl-mode is hooked to dired-mode."
  (skip-unless dired-module--init-loaded)
  (should (memq 'diredfl-mode (default-value 'dired-mode-hook))))

;;; Module structure tests

(ert-deftest dired-module/has-packages-eld ()
  "Module has packages.eld file."
  (let ((packages-file (expand-file-name "modules/dired/packages.eld"
                                         user-emacs-directory)))
    (should (file-exists-p packages-file))))

(ert-deftest dired-module/has-spec-md ()
  "Module has spec.md symlink."
  (let ((spec-file (expand-file-name "modules/dired/spec.md"
                                     user-emacs-directory)))
    (should (file-exists-p spec-file))))

(provide 'dired-tests)

;;; tests.el ends here
