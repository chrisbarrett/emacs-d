;;; may-i.el --- Major mode for may-i configuration files -*- lexical-binding: t; -*-

;;; Commentary:

;; `may-i-config-mode' is a major mode for `may-i' configuration files —
;; a Lisp-shaped DSL describing authorisation rules.  The mode derives
;; from `lisp-data-mode' and layers may-i-aware faces, font-lock,
;; indentation, apheleia formatter wiring, and an imenu index composing
;; Rules, Parsers, Arg styles, Definitions, Checks, Loads, and Safe env
;; vars.

;;; Code:

(require 'rx)


;;; Customisation group

(defgroup may-i nil
  "Major mode for may-i configuration files."
  :group 'languages
  :prefix "may-i-")


;;; Faces

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
  '((t :inherit font-lock-doc-face :slant normal))
  "Face for the reason string of `(allow|ask|deny …)' verbs."
  :group 'may-i)


;;; Surface-token lists

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


;;; Font-lock helpers

(defun may-i--head-rx (heads)
  "Match HEADS at the head of a call form.  Capture group 1 is the head."
  (rx-to-string `(seq "(" (group (or ,@heads)) symbol-end)))

(defun may-i--symbol-rx (symbols)
  "Match SYMBOLS as a standalone symbol.  Capture group 1 is the symbol."
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
LIMIT bounds the search.  `syntax-ppss' mutates match data, hence
`save-match-data'."
  (let (hit)
    (while (and (not hit) (re-search-forward may-i--reason-rx limit t))
      (unless (save-match-data (may-i--inside-check-p (match-beginning 0)))
        (setq hit t)))
    hit))

(defconst may-i--decision-rx
  (rx "(" (group (or "allow" "ask" "deny")) symbol-end))

(defun may-i--inside-check-decision-matcher (limit)
  "Font-lock MATCHER for decision verbs inside `(check …)' forms.
LIMIT bounds the search."
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

    ;; `(rule …)' PROG: a bare string or `(or "x" "y" …)'.  Fontify each
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


;;; Imenu

(defconst may-i--imenu-section-order
  '("Rules" "Parsers" "Arg styles" "Definitions"
    "Checks" "Loads" "Safe env vars")
  "Order in which imenu sections appear.")

(defun may-i--imenu-skip-p (pos)
  "Non-nil if POS is inside a string or comment.
`syntax-ppss' may move point; the surrounding `save-excursion'
keeps the caller's traversal stable."
  (save-excursion
    (let ((ppss (syntax-ppss pos)))
      (or (nth 3 ppss) (nth 4 ppss)))))

(defun may-i--imenu-read-head ()
  "Return symbol or string at point, or nil on failure.
Point is positioned just after an open paren; reads the head atom
without advancing past surrounding whitespace state for the caller."
  (save-excursion
    (skip-syntax-forward " >")
    (ignore-errors (read (current-buffer)))))

(defun may-i--imenu-read-next ()
  "Read the next sexp at point, returning nil on failure."
  (skip-syntax-forward " >")
  (ignore-errors (read (current-buffer))))

(defun may-i--imenu-collect-rule (open-pos sections)
  "Collect entries for a `(rule …)' form open at OPEN-POS into SECTIONS.
SECTIONS is an alist of (NAME . ENTRIES); returns the updated alist."
  (save-excursion
    (goto-char (1+ open-pos))
    (forward-sexp)                      ; past `rule'
    (let ((arg (may-i--imenu-read-next))
          names)
      (cond
       ((stringp arg)
        (push arg names))
       ((and (consp arg) (eq (car arg) 'or))
        (dolist (alt (cdr arg))
          (when (stringp alt) (push alt names)))))
      (dolist (name (nreverse names))
        (let* ((cell (or (assoc "Rules" sections)
                         (let ((c (cons "Rules" nil)))
                           (push c sections) c))))
          (setcdr cell (append (cdr cell) (list (cons name open-pos))))))))
  sections)

(defun may-i--imenu-collect-named (open-pos head section sections)
  "Collect entry for HEAD-NAMED form open at OPEN-POS into SECTION.
HEAD identifies the form head (e.g. `parser'); SECTIONS is the
running alist.  Returns the updated SECTIONS alist."
  (save-excursion
    (goto-char (1+ open-pos))
    (forward-sexp)                      ; past head
    (let ((name (may-i--imenu-read-next)))
      (when (symbolp name)
        (let* ((label (symbol-name name))
               (cell (or (assoc section sections)
                         (let ((c (cons section nil)))
                           (push c sections) c))))
          (setcdr cell (append (cdr cell) (list (cons label open-pos))))))))
  (ignore head)
  sections)

(defun may-i--imenu-collect-check (open-pos index sections)
  "Collect entry for a `(check …)' form open at OPEN-POS.
INDEX is the 1-based index of this check.  Returns the updated SECTIONS."
  (save-excursion
    (goto-char (1+ open-pos))
    (forward-sexp)                      ; past `check'
    (let* ((arg (may-i--imenu-read-next))
           (label (if (stringp arg) arg (format "check (%d)" index)))
           (cell (or (assoc "Checks" sections)
                     (let ((c (cons "Checks" nil)))
                       (push c sections) c))))
      (setcdr cell (append (cdr cell) (list (cons label open-pos))))))
  sections)

(defun may-i--imenu-collect-load (open-pos sections)
  "Collect entry for a `(load …)' form open at OPEN-POS."
  (save-excursion
    (goto-char (1+ open-pos))
    (forward-sexp)                      ; past `load'
    (let ((arg (may-i--imenu-read-next)))
      (when (stringp arg)
        (let* ((cell (or (assoc "Loads" sections)
                         (let ((c (cons "Loads" nil)))
                           (push c sections) c))))
          (setcdr cell (append (cdr cell) (list (cons arg open-pos))))))))
  sections)

(defun may-i--imenu-collect-safe-env-vars (open-pos sections)
  "Collect entry for a `(safe-env-vars …)' form open at OPEN-POS."
  (let* ((cell (or (assoc "Safe env vars" sections)
                   (let ((c (cons "Safe env vars" nil)))
                     (push c sections) c))))
    (setcdr cell (append (cdr cell) (list (cons "safe-env-vars" open-pos)))))
  sections)

(defun may-i--imenu-index-function ()
  "Build an imenu index for `may-i-config-mode' buffers.
Walks every open paren at any depth, dispatching by head symbol.
Entries are positioned at the open paren of their enclosing form.
Forms inside strings or comments are skipped."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((sections nil)
            (check-index 0))
        (while (re-search-forward "(" nil t)
          (let ((open-pos (1- (point))))
            (unless (may-i--imenu-skip-p open-pos)
              (let ((head (save-excursion
                            (goto-char (1+ open-pos))
                            (may-i--imenu-read-head))))
                (pcase head
                  ('rule
                   (setq sections (may-i--imenu-collect-rule open-pos sections)))
                  ('parser
                   (setq sections (may-i--imenu-collect-named
                                   open-pos 'parser "Parsers" sections)))
                  ('define-arg-style
                   (setq sections (may-i--imenu-collect-named
                                   open-pos 'define-arg-style "Arg styles" sections)))
                  ('define
                   (setq sections (may-i--imenu-collect-named
                                   open-pos 'define "Definitions" sections)))
                  ('check
                   (cl-incf check-index)
                   (setq sections (may-i--imenu-collect-check
                                   open-pos check-index sections)))
                  ('load
                   (setq sections (may-i--imenu-collect-load open-pos sections)))
                  ('safe-env-vars
                   (setq sections (may-i--imenu-collect-safe-env-vars
                                   open-pos sections))))))))
        (delq nil
              (mapcar (lambda (name)
                        (let ((cell (assoc name sections)))
                          (and cell (cdr cell) cell)))
                      may-i--imenu-section-order))))))


;;; Mode definition

;;;###autoload
(define-derived-mode may-i-config-mode lisp-data-mode "Lisp-Data [may-i]"
  "Major-mode for may-i configuration files."
  (setq-local font-lock-defaults '(may-i-font-lock-keywords))
  (setq-local imenu-create-index-function #'may-i--imenu-index-function))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/may-i/" (+? anychar) ".lisp" eos)
                                    'may-i-config-mode))

;;;###autoload
(add-to-list 'auto-mode-alist (cons (rx "/.may-i" (? ".local") ".lisp" eos)
                                    'may-i-config-mode))

(with-eval-after-load 'apheleia
  (defvar apheleia-formatters)
  (defvar apheleia-mode-alist)
  (add-to-list 'apheleia-formatters '(may-i . ("may-i" "fmt" "-")))
  (add-to-list 'apheleia-mode-alist '(may-i-config-mode . may-i)))


;;; Indent rules

(put 'cond 'lisp-indent-function 0)
(put 'define 'lisp-indent-function 1)
(put 'define-arg-style 'lisp-indent-function 1)
(put 'parser 'lisp-indent-function 1)
(put 'rule 'lisp-indent-function 1)
(put 'when 'lisp-indent-function 1)
(put 'unless 'lisp-indent-function 1)
(put 'with-facts 'lisp-indent-function 1)
(put 'defcontext 'lisp-indent-function 1)

(provide 'may-i)

;;; may-i.el ends here
