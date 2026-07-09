;;; lang-swift/init.el --- Swift language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Swift development with Tree-sitter mode and LSP integration.
;; - swift-ts-mode (requires the bundled tree-sitter-swift grammar)
;; - LSP via eglot with sourcekit-lsp (ships with the Xcode toolchain)
;; - Format-on-save via apheleia using swift-format (`xcrun swift-format')

;;; Code:

(require '+autoloads)

(require '+corelib)
(require '+lang)

(use-package swift-ts-mode :ensure nil
  :mode (("\\.swift\\'" . swift-ts-mode)
         ;; Generated module interfaces from sourcekit-lsp, e.g.
         ;; .../GeneratedInterfaces/Foundation.swiftinterface
         ("\\.swiftinterface\\'" . swift-ts-mode))
  :config
  ;; Generated interfaces are read-only; eglot still manages them for
  ;; navigation, but edits are blocked.
  (add-hook! 'swift-ts-mode-hook
    (when (string-suffix-p ".swiftinterface" (or buffer-file-name ""))
      (read-only-mode +1))))

;; LSP for editable source and generated interfaces alike, so xref can jump
;; between symbols inside .swiftinterface buffers.
(+lang-declare 'swift-ts-mode
               :lsp t
               :formatter '(swift-format . ("xcrun" "swift-format" "format" "-")))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(swift-ts-mode . ("sourcekit-lsp"))))

;;; init.el ends here
