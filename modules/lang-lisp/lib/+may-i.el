;;; +may-i.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(define-derived-mode may-i-config-mode lisp-data-mode "Lisp-Data [may-i]"
  "Major-mode for may-i configuration files."
  (setcar font-lock-defaults
          (append '(lisp-el-font-lock-keywords
                    lisp-el-font-lock-keywords-1
                    lisp-el-font-lock-keywords-2)
                  ;; Emacs 31
                  (when (boundp 'elisp-semantic-font-lock-keywords)
                    '(elisp-semantic-font-lock-keywords)))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/may-i/" (+? any) ".lisp" eos)
                                    'may-i-config-mode))

(put 'defcontext 'lisp-indent-function 1)
(put 'define 'lisp-indent-function 1)
(put 'rule 'lisp-indent-function 1)
(put 'with-facts 'lisp-indent-function 1)

;;; +may-i.el ends here
