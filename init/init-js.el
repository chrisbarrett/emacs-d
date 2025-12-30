;;; init-js.el --- JavaScript & TypeScript -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package js
  :mode ("\\.[mc]?js" . js-ts-mode))



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

;;; Select appropriate language server based on project type
;;
;; See also:
;;   https://www.gnu.org/software/emacs/manual/html_node/eglot/JSONRPC-objects-in-Elisp.html

(use-package eglot
  :hook (typescript-ts-mode-hook . eglot-ensure)

  :preface

  (defun +ts-project-type (&optional dir)
    (catch 'hit
      (locate-dominating-file (or dir default-directory)
                              (lambda (dir)
                                (when-let* ((type
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
                                               'node))))
                                  (throw 'hit type))))))

  (defun +ts-shebang-type ()
    (when-let* ((line
                 (ignore-errors
                   (save-excursion
                     (goto-char (point-min))
                     (when (looking-at-p (rx bol "#!"))
                       (buffer-substring (point-min) (line-end-position))))))
                (parts (string-split line "[ /]" t)))
      (pcase (car (nreverse parts))
        ("deno" 'deno)
        ("bun" 'bun)
        ("node" 'node))))


  (defun +ts-server-program (&rest _)
    (pcase (or
            (+ts-shebang-type)
            (+ts-project-type)
            'node)
      ('deno
       '("deno" "lsp"
         :initializationOptions
         '(:enable t
           :lint t
           :unstable t)))
      ((or 'bun 'node)
       '("typescript-language-server" "--stdio"))))

  :config

  (alist-set! eglot-server-programs
              '((js-mode :language-id "javascript")
                (js-ts-mode :language-id "javascript")
                (tsx-ts-mode :language-id "typescriptreact")
                (typescript-mode :language-id "typescript")
                (typescript-ts-mode :language-id "typescript"))
              '+ts-server-program))

;; Configure file templates.

(use-package +file-templates
  :config
  (eval-and-compile

    (defun +cdk-project-p (&optional dir)
      (locate-dominating-file (or dir default-directory) "cdk.json"))

    (defun +index-ts-p (_file)
      (equal "index.ts" (file-name-nondirectory (buffer-file-name)))))

  (+define-file-template-dispatcher 'typescript-ts-mode
    ((and (string-match-p "construct" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/construct.eld")
    ((and (string-match-p "/stacks/" (buffer-file-name))
          (+cdk-project-p)
          (not (+index-ts-p (buffer-file-name))))
     "cdk/stack.eld")))

(provide 'init-js)

;;; init-js.el ends here
