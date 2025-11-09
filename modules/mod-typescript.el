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

(with-eval-after-load 'eglot
  ;; Dynamically determine which server to use based on project type.

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

  (alist-set! eglot-server-programs
              '((js-mode :language-id "javascript")
                (js-ts-mode :language-id "javascript")
                (tsx-ts-mode :language-id "typescriptreact")
                (typescript-mode :language-id "typescript")
                (typescript-ts-mode :language-id "typescript"))
              '+ts-server-program))



(with-eval-after-load 'project
  (pushnew! project-vc-ignores ".nx/")
  (pushnew! project-vc-extra-root-markers
            "nx.json" "cdk.json"
            "deno.json" "deno.jsonc"
            "bun.lockb" "bunfig.toml"))



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
