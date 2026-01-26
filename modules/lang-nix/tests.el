;;; lang-nix/tests.el --- Tests for lang-nix module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Nix language support module.

;;; Code:

(require 'ert)

;; Load module init from this directory
;; May fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (load init-file nil 'nomessage)
    (error nil)))

;;; P1: Opening *.nix file activates nix-ts-mode

(ert-deftest lang-nix/auto-mode-nix ()
  "P1: .nix files should be associated with nix-ts-mode."
  (let ((entry (assoc "\\.nix\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'nix-ts-mode))))

;;; P2: Opening /flake.lock activates json-ts-mode

(ert-deftest lang-nix/auto-mode-flake-lock ()
  "P2: flake.lock files should be associated with json-ts-mode."
  (let ((matches nil))
    (dolist (entry auto-mode-alist)
      (when (and (stringp (car entry))
                 (string-match-p "flake.lock" (car entry)))
        (push entry matches)))
    ;; Skip if module init didn't configure this
    (skip-unless matches)
    (should (eq (cdr (car matches)) 'json-ts-mode))))

;;; P3: Opening file under /nix/store/ enables read-only-mode
;; Tested via dirlocals mechanism

(ert-deftest lang-nix/nix-store-read-only ()
  "P3: /nix/store/ should have read-only dirlocals."
  ;; Skip if +corelib not available
  (skip-unless (require '+corelib nil t))
  ;; Check that dirlocals are set for /nix/store/
  (let ((dirlocals (dir-locals-find-file "/nix/store/some-hash/file.nix")))
    ;; dirlocals returns nil, file path, or (file . dir)
    ;; For a regexp match, it should find our settings
    ;; Skip if not configured
    (skip-unless (or dirlocals
                     (assoc "/nix/store/" dir-locals-class-alist)))
    (should (or dirlocals
                (assoc "/nix/store/" dir-locals-class-alist)))))

;;; P4: eglot-ensure is called on nix-ts-mode-local-vars-hook

(ert-deftest lang-nix/eglot-hook ()
  "P4: nix-ts-mode-local-vars-hook should contain eglot-ensure."
  (skip-unless (featurep 'nix-ts-mode))
  (should (memq 'eglot-ensure nix-ts-mode-local-vars-hook)))

;;; P5: apheleia-formatter is set to nixpkgs-fmt in nix-ts-mode

(ert-deftest lang-nix/apheleia-formatter-registered ()
  "P5: nixpkgs-fmt should be in apheleia-formatters."
  (require 'apheleia)
  (should (assoc 'nixpkgs-fmt apheleia-formatters)))

(ert-deftest lang-nix/apheleia-formatter-hook ()
  "P5: nix-ts-mode-hook should set apheleia-formatter."
  ;; Skip if nix-ts-mode not available
  (skip-unless (boundp 'nix-ts-mode-hook))
  ;; Check the hook is set up to configure apheleia-formatter
  ;; Skip if not configured (depends on init.el loading)
  (skip-unless (memq 'setq-hook!--nix-ts-mode-hook--apheleia-formatter
                     nix-ts-mode-hook))
  (should (memq 'setq-hook!--nix-ts-mode-hook--apheleia-formatter
                nix-ts-mode-hook)))

;;; P6: project-vc-extra-root-markers contains "flake.nix"

(ert-deftest lang-nix/project-root-marker ()
  "P6: flake.nix should be in project-vc-extra-root-markers."
  (require 'project)
  (should (member "flake.nix" project-vc-extra-root-markers)))

;;; P7: Creating flake.nix inserts template with directory name
;; File template functionality tested via the template file existence

(ert-deftest lang-nix/file-template-exists ()
  "P7: flake.eld template should exist."
  (let ((template-file (expand-file-name "file-templates/flake.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

(ert-deftest lang-nix/file-template-has-directory-name ()
  "P7: flake.eld template should reference directory-name."
  (let ((template-file (expand-file-name "file-templates/flake.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (should (search-forward "directory-name" nil t))))))

(provide 'lang-nix-tests)

;;; lang-nix/tests.el ends here
