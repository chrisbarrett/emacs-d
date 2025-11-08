;;; mod-typescript.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'cl-lib)

(cl-eval-when (compile)
  (require 'eglot))


;;; Select appropriate language server based on project type


(with-eval-after-load 'eglot
  ;; https://docs.deno.com/runtime/getting_started/setup_your_environment/

  (add-to-list 'eglot-server-programs '((js-mode typescript-mode typescript-ts-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((_server eglot-deno))
    (list
     :enable t
     :unstable t
     :typescript
     (:inlayHints
      (:variableTypes
       (:enabled t))
      (:parameterTypes
       (:enabled t))))))



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
