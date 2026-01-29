;;; init.el --- JavaScript & TypeScript configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; JavaScript and TypeScript development with automatic LSP selection
;; based on project type (Deno, Bun, Node).

;;; Code:

(require '+autoloads)

(require '+corelib)

;; JavaScript mode associations
(use-package js
  :mode ("\\.[mc]?js\\'" . js-ts-mode))

;; Node modules read-only
(+dirlocals-set-regexp (rx "/node_modules/")
  '((nil . ((mode . read-only)))))

;; TypeScript mode with shebang detection
(use-package typescript-ts-mode
  :init
  ;; Activate on shebangs for node, deno, bun etc.
  (add-to-list 'magic-mode-alist (cons (rx bol "#!" (+? nonl) (or "/" " ") (or "deno" "node" "bun" "tsx")
                                           (* space) (or eol (and "--" (+ nonl))))
                                       'typescript-ts-mode))
  :config
  (pushnew! find-sibling-rules
            ;; Tests -> impl
            (list (rx (group (+? any)) (or ".test" ".integration") ".ts" eos)
                  (rx (backref 1) ".ts"))
            ;; Impl -> tests
            (list (rx (group (+? any)) ".ts" eos)
                  (rx (backref 1) ".test.ts")
                  (rx (backref 1) ".integration.ts"))))

;; Define extra typescript project types
(use-package project
  :config
  (pushnew! project-vc-ignores ".nx/")
  (pushnew! project-vc-extra-root-markers
            "nx.json" "cdk.json"
            "deno.json" "deno.jsonc"
            "bun.lockb" "bunfig.toml"))

;; Select appropriate language server based on project type
(use-package eglot
  :hook (typescript-ts-mode-local-vars-hook . eglot-ensure)
  :config
  (alist-set! eglot-server-programs
              '((js-mode :language-id "javascript")
                (js-ts-mode :language-id "javascript")
                (tsx-ts-mode :language-id "typescriptreact")
                (typescript-mode :language-id "typescript")
                (typescript-ts-mode :language-id "typescript"))
              '+ts-server-program))

;; Configure file templates
(use-package +file-templates
  :config
  (defun +index-ts-p (_file)
    "Return non-nil if current buffer is an index.ts file."
    (equal "index.ts" (file-name-nondirectory (buffer-file-name))))

  (+define-file-template-dispatcher 'typescript-ts-mode
    ((and (string-match-p "construct" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/construct.eld")
    ((and (string-match-p "/stacks/" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/stack.eld")))


