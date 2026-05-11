;;; +may-i.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defconst may-i-def-keywords
  '("define-arg-style" "define" "parser" "rule"))

(defconst may-i-top-level-forms
  '("check" "load" "safe-env-vars"))

(defconst may-i-control-keywords
  '("and" "cond" "if" "not" "or" "unless" "when" "with-facts"))

(defconst may-i-parser-attributes
  '("after" "combined-shorts" "first-token-bundle" "long-prefix"
    "many-till" "overrides" "pun" "separators" "short-prefix" "style"))

(defconst may-i-style-names
  '("gnu" "key-value" "legacy-bundle" "single-dash-long"))

(defconst may-i-constants '("else" "*"))

(defface may-i-allow-face
  '((t :inherit success :weight semibold :slant normal))
  "Face for `(allow …)' decision verbs in may-i configs."
  :group 'may-i)

(defface may-i-ask-face
  '((t :inherit warning :weight semibold :slant normal))
  "Face for `(ask …)' decision verbs in may-i configs."
  :group 'may-i)

(defface may-i-deny-face
  '((t :inherit error :weight semibold :slant normal))
  "Face for `(deny …)' decision verbs in may-i configs."
  :group 'may-i)

(defface may-i-reason-face
  '((t :inherit font-lock-doc-face :slant italic))
  "Face for the reason string of `(allow|ask|deny …)' verbs."
  :group 'may-i)

(defun may-i--head-rx (heads)
  "Match HEADS at the head of a call form. Capture group 1 is the head."
  (rx-to-string `(seq "(" (group (or ,@heads)) symbol-end)))

(defun may-i--symbol-rx (symbols)
  "Match SYMBOLS as a standalone symbol. Capture group 1 is the symbol."
  (rx-to-string `(seq symbol-start (group (or ,@symbols)) symbol-end)))

(defun may-i--inside-check-p (pos)
  "Non-nil if POS is inside a `(check …)' form at any nesting depth."
  (save-excursion
    (catch 'found
      (dolist (open (nth 9 (syntax-ppss pos)))
        (goto-char (1+ open))
        (when (looking-at-p (rx "check" symbol-end))
          (throw 'found t))))))

(defconst may-i--reason-rx
  (rx "(" (or "allow" "ask" "deny") symbol-end
      (* whitespace)
      (group "\""
             (* (or (not (any "\"" "\\"))
                    (seq "\\" anychar)))
             "\"")))

(defun may-i--reason-matcher (limit)
  "Font-lock MATCHER for reason strings outside `(check …)' forms.
`syntax-ppss' mutates match data, hence `save-match-data'."
  (let (hit)
    (while (and (not hit) (re-search-forward may-i--reason-rx limit t))
      (unless (save-match-data (may-i--inside-check-p (match-beginning 0)))
        (setq hit t)))
    hit))

(defconst may-i--decision-rx
  (rx "(" (group (or "allow" "ask" "deny")) symbol-end))

(defun may-i--inside-check-decision-matcher (limit)
  "Font-lock MATCHER for decision verbs inside `(check …)' forms."
  (let (hit)
    (while (and (not hit) (re-search-forward may-i--decision-rx limit t))
      (when (save-match-data (may-i--inside-check-p (match-beginning 0)))
        (setq hit t)))
    hit))

(defun may-i--rule-prog-limit ()
  "PRE-MATCH-FORM for `(rule …)' head fontification.
Skip whitespace after the matched `(rule', then return the end of the
first argument — either a bare string or an `(or …)' form."
  (skip-syntax-forward " >")
  (when (memq (char-after) '(?\" ?\())
    (save-excursion (ignore-errors (forward-sexp)) (point))))

(defconst may-i--string-rx
  (rx "\""
      (* (or (not (any "\"" "\\"))
             (seq "\\" anychar)))
      "\""))

(defun may-i--string-matcher (limit)
  "Anchored MATCHER for the next string literal up to LIMIT."
  (re-search-forward may-i--string-rx limit t))

(defconst may-i-font-lock-keywords
  `(;; Def forms: head + name being defined.
    (,(rx-to-string
       `(seq "(" (group (or ,@may-i-def-keywords)) symbol-end
             (* whitespace)
             (? (group (+ (or word (syntax symbol)))))))
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    ;; Other top-level forms.
    (,(may-i--head-rx may-i-top-level-forms)
     (1 font-lock-keyword-face))

    ;; `(rule …)' PROG: a bare string or `(or "x" "y" …)'. Fontify each
    ;; string as a function name.
    (,(rx "(" "rule" symbol-end)
     (may-i--string-matcher
      (may-i--rule-prog-limit)
      nil
      (0 'font-lock-function-name-face t)))

    ;; Control flow / logical combinators (call position only — `else'
    ;; below covers the cond-clause use of a bare symbol).
    (,(may-i--head-rx may-i-control-keywords)
     (1 font-lock-keyword-face))

    ;; Decision verbs.
    (,(may-i--head-rx '("allow")) (1 'may-i-allow-face))
    (,(may-i--head-rx '("ask"))   (1 'may-i-ask-face))
    (,(may-i--head-rx '("deny"))  (1 'may-i-deny-face))

    ;; Inside `(check …)' the verbs are claims, not decisions — drop
    ;; the bold weight, keep the colour.
    (may-i--inside-check-decision-matcher
     (1 '(:weight normal) prepend))

    ;; Reason string immediately following a decision verb, except
    ;; inside `(check …)' where it's a command-line under test.
    (may-i--reason-matcher (1 'may-i-reason-face t))

    ;; Recursion + regex stay as builtins.
    (,(may-i--head-rx '("authorise" "regex"))
     (1 font-lock-builtin-face))

    ;; Parser / arg-style attribute heads.
    (,(may-i--head-rx may-i-parser-attributes)
     (1 font-lock-type-face))

    ;; Style names (appear bare inside `(style …)').
    (,(may-i--symbol-rx may-i-style-names)
     (1 font-lock-constant-face))

    ;; Pattern atoms / `else'.
    (,(may-i--symbol-rx may-i-constants)
     (1 font-lock-constant-face))

    ;; Fact keywords like :env, :via, :ssh/host.
    (,(rx symbol-start ":" (+ (or word (syntax symbol) "/")))
     . font-lock-constant-face)))

;;;###autoload
(define-derived-mode may-i-config-mode lisp-data-mode "Lisp-Data [may-i]"
  "Major-mode for may-i configuration files."
  (setq-local font-lock-defaults '(may-i-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/may-i/" (+? any) ".lisp" eos)
                                    'may-i-config-mode))

(put 'check 'lisp-indent-function 0)
(put 'cond 'lisp-indent-function 0)
(put 'define 'lisp-indent-function 1)
(put 'define-arg-style 'lisp-indent-function 1)
(put 'parser 'lisp-indent-function 1)
(put 'rule 'lisp-indent-function 1)
(put 'when 'lisp-indent-function 1)
(put 'unless 'lisp-indent-function 1)
(put 'with-facts 'lisp-indent-function 1)

;;; +may-i.el ends here
