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

;;; Polymode — syntax highlighting for embedded languages in multiline strings
;;
;; Nix multiline strings use '' delimiters. Nixpkgs convention marks the
;; embedded language with a comment before the string:
;;   buildPhase = /* bash */ ''
;;   buildPhase =
;;     # bash
;;     ''

(use-package polymode
  :defer t
  :init
  (defun +nix-poly--head-matcher (lang)
    "Return head-matcher regex for LANG annotation before a Nix multiline string.
Matches both `/* lang */' block comments and `# lang' line comments,
with `''' potentially on the same or next line."
    (rx-to-string
     `(seq (or (seq "/*" (+ space) ,lang (+ space) "*/")
               (seq "# " ,lang))
           (* space) (? "\n" (* space))
           "''")))

  (define-hostmode poly-nix-ts-hostmode
    :mode 'nix-ts-mode)

  (define-innermode poly-nix-bash-innermode
    :mode 'bash-ts-mode
    :head-matcher (+nix-poly--head-matcher "bash")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-python-innermode
    :mode 'python-ts-mode
    :head-matcher (+nix-poly--head-matcher "python")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-elisp-innermode
    :mode 'emacs-lisp-mode
    :head-matcher (+nix-poly--head-matcher "elisp")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-lua-innermode
    :mode 'lua-ts-mode
    :head-matcher (+nix-poly--head-matcher "lua")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-json-innermode
    :mode 'json-ts-mode
    :head-matcher (+nix-poly--head-matcher "json")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-sql-innermode
    :mode 'sql-mode
    :head-matcher (+nix-poly--head-matcher "sql")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-rust-innermode
    :mode 'rust-ts-mode
    :head-matcher (+nix-poly--head-matcher "rust")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-nix-c-innermode
    :mode 'c-ts-mode
    :head-matcher (+nix-poly--head-matcher "c")
    :tail-matcher (rx bol (* space) "''")
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-nix-ts-mode
    :hostmode 'poly-nix-ts-hostmode
    :innermodes '(poly-nix-bash-innermode
                  poly-nix-python-innermode
                  poly-nix-elisp-innermode
                  poly-nix-lua-innermode
                  poly-nix-json-innermode
                  poly-nix-sql-innermode
                  poly-nix-rust-innermode
                  poly-nix-c-innermode))

  (add-to-list 'major-mode-remap-alist '(nix-ts-mode . poly-nix-ts-mode))

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

(defun +nix--multiline-head-valid-p (beg)
  "Return non-nil if overlay at BEG still matches a Nix multiline annotation."
  (save-excursion
    (goto-char beg)
    (looking-at (rx (or (seq "/*" (+ space) (+ (any "A-Za-z_-")) (+ space) "*/")
                        (seq "#" (+ space) (+ (any "A-Za-z_-"))))))))

(with-eval-after-load '+code-fences
  (+code-fences-register 'nix-ts-mode
                         :head-valid-p #'+nix--multiline-head-valid-p
                         :unquoted-p (lambda (_beg _end) t)
                         :interpolation-fn #'+nix--add-interpolation-overlays))

;;; init.el ends here
