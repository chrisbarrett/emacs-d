;;; init.el --- LaTeX editing configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; LaTeX support with format-on-save via latexindent.

;;; Code:

(require '+autoloads)

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


