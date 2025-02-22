emacs-lisp-mode

(s "(setq " p ")")
(sl "(setq-local " p ")")
(l "(let (" p ")" n> q ")")
(la "(lambda (" p ")" n> q ")")

(i "(if " q ")")
(w "(when " q ")")
(u "(unless " q ")")
(wh "(while " p n> q ")")
(c "(cond" n> "(" q ")" n> ")")
(wl "(when-let* ((" p "))" n> q ")")
(il "(if-let* ((" p "))" n> q ")")
(cc "(condition-case err" n> p n> "(err " q "))")

(p "(progn" n> q ")")
(p1 "(prog1 " p n> q ")")

(dl "(dolist (" p ")" n> q ")")

(pc "(pcase " q n> ")")
(pce "(pcase-exhaustive " q n> ")")
(pdl "(pcase-dolist (" p ")" n> q ")")
(pl "(pcase-let (" p ")" n> q ")")
(pla "(pcase-lambda (" p ")" n> q ")")

(v "(defvar " q ")")
(dc "(defconst " q ")")
(d "(defun " p " (" p ")" n> q ")")
(dm "(defmacro " p " (" p ")" n> q ")")

(atl "(add-to-list '" q ")")
(as "(cl-assert " p " t " "\"" p "\")")

(a "(with-eval-after-load '" q ")")
(ewc "(eval-when-compile" n> q ")")
(eac "(eval-and-compile" n> q ")")
(r "(require '" q ")")
(al "(autoload '" (p "Identifier" sym) " "
    "\""
    (if-let* ((file (symbol-file (if (stringp sym) (intern sym) sym))))
        (file-name-sans-extension (file-name-nondirectory file))
      "")
    "\")")

(lbp "(line-beginning-position)")
(lep "(line-end-position)")
(pm "(point-min)")
(pM "(point-max)")
(wcb "(with-current-buffer " p n> q ")")
(bs "(buffer-substring " q ")")
(bsl "(buffer-substring (line-beginning-position) (line-end-position))")

(m "(message \"" p "\" " q ")")

(up "(use-package " q ")")
(in "(interactive)")
(gc "(goto-char " q ")")
