;;; lib.el --- JavaScript/TypeScript library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Project type detection and LSP server selection for JS/TS development.

;;; Code:

;;;###autoload
(defun +ts-project-type (&optional dir)
  "Detect project type (deno/bun/node) by marker files.
Search from DIR or `default-directory' upward."
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

;;;###autoload
(defun +ts-shebang-type ()
  "Detect type from shebang line."
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

;;;###autoload
(defun +ts-server-program (&rest _)
  "Return appropriate LSP server command based on project type."
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

;;;###autoload
(defun +cdk-project-p (&optional dir)
  "Return non-nil if DIR (or `default-directory') is a CDK project."
  (locate-dominating-file (or dir default-directory) "cdk.json"))



;;; lib.el ends here
