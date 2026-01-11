;;; init-latex.el --- Configuration for latex editing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package apheleia
  :after tex-mode
  :config
  ;; Set latexindent's indent style based on `indent-tabs-mode'.
  (alist-set! apheleia-formatters 'latexindent
              '("latexindent" "--logfile=/dev/null"
                (when apheleia-formatters-respect-indent-level
                  (if indent-tabs-mode
                      "-y=defaultIndent:\"\\t\""
                    "-y=defaultIndent:\"  \"")))))

(provide 'init-latex)

;;; init-latex.el ends here
