;;; init.el --- LaTeX editing configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; LaTeX support with format-on-save via latexindent.

;;; Code:

(require '+autoloads)

(require '+corelib)
(require '+lang)

;; latexindent is a built-in apheleia formatter; override its command to
;; silence the logfile and set the indent style from `indent-tabs-mode'.
;; No mode association is declared: apheleia already maps the TeX modes to
;; latexindent, so this registers the command definition only.
(+lang-declare nil
               :formatter '(latexindent
                            . ("latexindent" "--logfile=/dev/null"
                               (when apheleia-formatters-respect-indent-level
                                 (if indent-tabs-mode
                                     "-y=defaultIndent:\"\\t\""
                                   "-y=defaultIndent:\"  \"")))))



;;; init.el ends here
