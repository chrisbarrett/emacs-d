;;; mod-typescript.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'cl-lib)

(cl-eval-when (compile)
  (require 'eglot))


;;; Select appropriate language server based on project type
;;
;; See also:
;;   https://www.gnu.org/software/emacs/manual/html_node/eglot/JSONRPC-objects-in-Elisp.html

(defun +ts-project-type (&optional dir)
  (locate-dominating-file (or dir default-directory)
                          (lambda (dir)
                            (cond
                             ((file-exists-p (file-name-concat dir "deno.json"))
                              'deno)
                             ((file-exists-p (file-name-concat dir "deno.jsonc"))
                              'deno)
                             ((file-exists-p (file-name-concat dir "bun.lockb"))
                              'bun)
                             ((file-exists-p (file-name-concat dir "bunfig.toml"))
                              'bun)
                             ((file-exists-p (file-name-concat dir "package.json"))
                              'node)))))

(with-eval-after-load 'eglot

  ;; Deno LSP configuration

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "Deno LSP")

  (cl-defmethod eglot-initialization-options ((_server eglot-deno))
    (list
     :enable t
     :lint t
     :unstable t))

  ;; Standard TypeScript LSP configuration

  (defclass eglot-typescript (eglot-lsp-server) ()
    :documentation "TypeScript LSP")

  ;; https://www.npmjs.com/package/typescript-language-server/v/2.3.0#initializationoptions
  (cl-defmethod eglot-initialization-options ((_server eglot-typescript))
    (list
     :enable t
     :unstable t
     :lint t))

  ;; Dynamically determine which server to use based on project type.

  (defun +ts-server-program (&rest _)
    (pcase (or (+ts-project-type) 'node)
      ('deno
       '(eglot-deno "deno" "lsp"))
      ((or 'bun 'node)
       '(eglot-typescript "typescript-language-server" "--stdio"))))

  (add-to-list 'eglot-server-programs (cons '((js-mode :language-id "javascript")
                                              (js-ts-mode :language-id "javascript")
                                              (tsx-ts-mode :language-id "typescriptreact")
                                              (typescript-mode :language-id "typescript")
                                              (typescript-ts-mode :language-id "typescript"))
                                            '+ts-server-program)))



(with-eval-after-load 'project
  (pushnew! project-vc-ignores ".nx/")
  (pushnew! project-vc-extra-root-markers "nx.json" "cdk.json"))



(pushnew! find-sibling-rules
          ;; Tests -> impl
          (list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
                (rx (backref 1) ".ts"))
          ;; Impl -> tests
          (list (rx (group (+? any)) ".ts" eos)
                (rx (backref 1) ".test.ts")
                (rx (backref 1) ".integration.ts")))

(provide 'mod-typescript)

;;; mod-typescript.el ends here
