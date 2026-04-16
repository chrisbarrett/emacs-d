;;; lang-nix/init.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix development with Tree-sitter mode and LSP integration.
;; - LSP via eglot with nil or nixd
;; - nixpkgs-fmt formatter via apheleia
;; - Read-only protection for /nix/store/
;; - Project detection via flake.nix
;; - Polymode for syntax-highlighted multiline strings

;;; Code:

(require '+autoloads)
(require '+corelib)

;; Read-only protection for Nix store
(+dirlocals-set "/nix/store/"
  '((nil . ((mode . read-only)))))

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode-local-vars-hook . eglot-ensure)
  :init
  ;; flake.lock files are JSON
  (add-to-list 'auto-mode-alist (cons (rx "/flake.lock" eos) #'json-ts-mode)))

(use-package project
  :config
  (pushnew! project-vc-extra-root-markers "flake.nix"))

;; File template for flake.nix
(+define-file-template (rx "flake.nix" eos) "flake.eld")

;;; Tree-sitter span detection for embedded languages in Nix multiline strings

(+load "lib/+nix-ts-spans.el")

;;; Polymode — syntax highlighting for embedded languages in multiline strings

(use-package polymode
  :defer t
  :init
  (define-hostmode poly-nix-ts-hostmode
    :mode 'nix-ts-mode)

  (define-auto-innermode poly-nix-ts-auto-innermode
    :head-matcher #'+nix-ts--head-matcher
    :tail-matcher #'+nix-ts--tail-matcher
    :mode-matcher #'+nix-ts--mode-matcher
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-nix-ts-mode
    :hostmode 'poly-nix-ts-hostmode
    :innermodes '(poly-nix-ts-auto-innermode))

  (defun +nix-ts--init-cache ()
    "Build span cache and register for parser notifications."
    (+nix-ts--rebuild-cache)
    (dolist (parser (treesit-parser-list))
      (when (eq (treesit-parser-language parser) 'nix)
        (treesit-parser-add-notifier parser #'+nix-ts--rebuild-cache))))

  (add-to-list 'major-mode-remap-alist '(nix-ts-mode . poly-nix-ts-mode))

  (add-hook 'poly-nix-ts-mode-hook #'+nix-ts--init-cache)
  (add-hook 'poly-nix-ts-mode-hook #'+polymode-refontify-inner-spans))


;;; Code fences — Nix-specific dispatch callbacks

(defun +nix--interpolation-escaped-p (pos)
  "Return non-nil if `$' at POS is escaped by Nix `\\='\\='$' syntax.
Count consecutive `\\='' chars before POS.  Even count >= 2 means
escaped; 0 or odd count means real interpolation."
  (let ((quotes 0))
    (save-excursion
      (goto-char pos)
      (while (eq (char-before (- (point) quotes)) ?')
        (cl-incf quotes)))
    (and (> quotes 0) (cl-evenp quotes))))

(defun +nix--add-interpolation-overlays (beg end base)
  "Add overlays for Nix `${...}' interpolation between BEG and END.
Skips escaped `\\='\\='${' sequences using single-quote parity.
Overlays are created in BASE buffer."
  (with-current-buffer base
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (rx "$" "{") end t)
        (let ((dollar-pos (match-beginning 0)))
          (unless (+nix--interpolation-escaped-p dollar-pos)
            (let ((open-brace (point))
                  (depth 1))
              (while (and (> depth 0)
                          (re-search-forward (rx (or "{" "}")) end t))
                (pcase (char-before)
                  (?{ (cl-incf depth))
                  (?} (cl-decf depth))))
              (when (zerop depth)
                (let ((ov (make-overlay dollar-pos (point) base)))
                  (overlay-put ov 'face '+polymode-interpolation-face)
                  (overlay-put ov '+polymode-fence t))))))))))

(with-eval-after-load '+code-fences
  (+code-fences-register 'nix-ts-mode
                         :head-valid-p #'+nix--multiline-head-valid-p
                         :unquoted-p (lambda (_beg _end) t)
                         :interpolation-fn #'+nix--add-interpolation-overlays
                         :count-openers #'+nix-ts--count-openers))

;;; init.el ends here
