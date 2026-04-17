;;; +nix-ts-spans.el --- Tree-sitter span detection for Nix multiline strings -*- lexical-binding: t; -*-

;;; Code:

(require 'treesit)

(defvar +nix-attrpath-mode-alist
  `((,(rx (or "shellHook"
              "buildPhase" "installPhase" "configurePhase"
              "checkPhase" "fixupPhase" "unpackPhase" "patchPhase"
              "preBuild" "postBuild" "preInstall" "postInstall"
              "preConfigure" "postConfigure" "preCheck" "postCheck"
              "preFixup" "postFixup" "script"
              "preStart" "postStart" "preStop" "postStop"
              "ExecStart" "ExecStartPre" "ExecStartPost"
              "ExecStop" "ExecStopPost" "ExecReload")
          eos)
     . bash-ts-mode))
  "Alist of (REGEXP . MODE-SYMBOL) for attrpath-based mode detection.")

(defvar +nix-lang-mode-alist
  '(("bash"   . bash-ts-mode)
    ("python" . python-ts-mode)
    ("elisp"  . emacs-lisp-mode)
    ("lua"    . lua-ts-mode)
    ("json"   . json-ts-mode)
    ("sql"    . sql-mode)
    ("rust"   . rust-ts-mode)
    ("c"      . c-ts-mode))
  "Alist mapping annotation language names to major mode symbols.")

(defvar-local +nix-ts--cached-spans nil
  "Sorted list of (HEAD-BEG HEAD-END TAIL-BEG TAIL-END MODE-SYMBOL ATTRPATH).")

(defun +nix-ts--parse-comment-lang (comment-text)
  "Extract language name from COMMENT-TEXT like \"/* bash */\" or \"# bash\"."
  (cond
   ((string-match (rx "/*" (+ space) (group (+ (any "A-Za-z_-"))) (+ space) "*/")
                  comment-text)
    (match-string 1 comment-text))
   ((string-match (rx "#" (+ space) (group (+ (any "A-Za-z_-"))))
                  comment-text)
    (match-string 1 comment-text))))

(defun +nix-ts--binding-attrpath (binding-node)
  "Build full dotted attrpath for BINDING-NODE by walking ancestors."
  (let (segments)
    (while (and binding-node
                (equal (treesit-node-type binding-node) "binding"))
      (let ((ap (treesit-node-child-by-field-name binding-node "attrpath")))
        (when ap
          (let (names
                (n (treesit-node-child-count ap t)))
            (dotimes (i n)
              (push (treesit-node-text (treesit-node-child ap i t)) names))
            (push (string-join (nreverse names) ".") segments))))
      (setq binding-node
            (let ((p (treesit-node-parent binding-node)))
              (while (and p (not (equal (treesit-node-type p) "binding")))
                (setq p (treesit-node-parent p)))
              p)))
    (string-join (nreverse segments) ".")))

(defun +nix-ts--resolve-comment-mode (comment-text)
  "Resolve COMMENT-TEXT to a mode symbol.
Tries `+nix-lang-mode-alist' first, then fboundp probe."
  (let ((lang (+nix-ts--parse-comment-lang comment-text)))
    (when lang
      (or (cdr (assoc lang +nix-lang-mode-alist))
          (let ((sym (intern (concat lang "-mode"))))
            (when (fboundp sym) sym))))))

(defun +nix-ts--resolve-attrpath-mode (attrpath)
  "Resolve ATTRPATH to a mode symbol via `+nix-attrpath-mode-alist'."
  (when attrpath
    (cl-loop for (regexp . mode) in +nix-attrpath-mode-alist
             when (string-match-p regexp attrpath)
             return mode)))

(defun +nix-ts--group-captures (captures fields)
  "Group CAPTURES into lists of alists keyed by FIELDS.
Each group is a complete set of FIELDS captured from one binding."
  (let (groups current)
    (dolist (capture captures)
      (let ((name (car capture))
            (node (cdr capture)))
        (when (memq name fields)
          (when (and current (assq name current))
            (push (nreverse current) groups)
            (setq current nil))
          (push (cons name node) current))))
    (when current
      (push (nreverse current) groups))
    (nreverse groups)))

(defun +nix-ts--scan-spans ()
  "Scan buffer for Nix bindings with indented string expressions.
Returns sorted list of (HEAD-BEG HEAD-END TAIL-BEG TAIL-END MODE-SYMBOL ATTRPATH)."
  (let* ((root (treesit-buffer-root-node))
         (query-annotated
          (treesit-query-compile
           'nix
           '((binding
              attrpath: (attrpath attr: (identifier) @attr)
              (comment) @lang-comment
              expression: (indented_string_expression) @str))))
         (query-plain
          (treesit-query-compile
           'nix
           '((binding
              attrpath: (attrpath attr: (identifier) @attr)
              expression: (indented_string_expression) @str))))
         (annotated-matches (treesit-query-capture root query-annotated))
         (plain-matches (treesit-query-capture root query-plain))
         (annotated-strs (make-hash-table :test 'eql))
         spans)
    ;; Process annotated bindings (comment resolution takes priority)
    (let ((groups (+nix-ts--group-captures annotated-matches '(attr lang-comment str))))
      (dolist (group groups)
        (let* ((comment-node (cdr (assq 'lang-comment group)))
               (str-node (cdr (assq 'str group)))
               (comment-text (treesit-node-text comment-node))
               (mode (+nix-ts--resolve-comment-mode comment-text))
               (binding-node (treesit-node-parent str-node))
               (attrpath (+nix-ts--binding-attrpath binding-node)))
          (when mode
            (let* ((str-start (treesit-node-start str-node))
                   (str-end (treesit-node-end str-node))
                   (head-beg (treesit-node-start comment-node))
                   (head-end (+ str-start 2))
                   (tail-beg (- str-end 2))
                   (tail-end str-end))
              (puthash str-start t annotated-strs)
              (push (list head-beg head-end tail-beg tail-end mode attrpath) spans))))))
    ;; Process plain bindings (attrpath fallback)
    (let ((groups (+nix-ts--group-captures plain-matches '(attr str))))
      (dolist (group groups)
        (let* ((attr-node (cdr (assq 'attr group)))
               (str-node (cdr (assq 'str group)))
               (str-start (treesit-node-start str-node)))
          (unless (gethash str-start annotated-strs)
            (let* ((binding-node (treesit-node-parent str-node))
                   (attrpath (+nix-ts--binding-attrpath binding-node))
                   (mode (+nix-ts--resolve-attrpath-mode attrpath)))
              (when mode
                (let* ((str-end (treesit-node-end str-node))
                       (head-beg (treesit-node-start attr-node))
                       (head-end (+ str-start 2))
                       (tail-beg (- str-end 2))
                       (tail-end str-end))
                  (push (list head-beg head-end tail-beg tail-end mode attrpath) spans))))))))
    (sort spans (lambda (a b) (< (car a) (car b))))))

(defun +nix-ts--rebuild-cache (&rest _)
  "Rebuild `+nix-ts--cached-spans' from current buffer tree-sitter parse."
  (setq +nix-ts--cached-spans (+nix-ts--scan-spans)))

;;; Polymode function matchers

(defun +nix-ts--head-matcher (ahead)
  "Find head span searching forward (AHEAD>0) or backward (AHEAD<0).
Returns (BEG . END) cons or nil."
  (let ((pos (point)))
    (if (> ahead 0)
        (cl-loop for span in +nix-ts--cached-spans
                 when (> (nth 0 span) pos)
                 return (cons (nth 0 span) (nth 1 span)))
      (cl-loop for span in (reverse +nix-ts--cached-spans)
               when (< (nth 0 span) pos)
               return (cons (nth 0 span) (nth 1 span))))))

(defun +nix-ts--tail-matcher (ahead)
  "Find tail span searching forward (AHEAD>0) or backward (AHEAD<0).
Returns (BEG . END) cons or nil."
  (let ((pos (point)))
    (if (> ahead 0)
        (cl-loop for span in +nix-ts--cached-spans
                 when (> (nth 2 span) pos)
                 return (cons (nth 2 span) (nth 3 span)))
      (cl-loop for span in (reverse +nix-ts--cached-spans)
               when (< (nth 2 span) pos)
               return (cons (nth 2 span) (nth 3 span))))))

(defun +nix-ts--mode-matcher ()
  "Return mode name string for the span at point.
Called by polymode at head-start position."
  (let ((pos (point)))
    (cl-loop for span in +nix-ts--cached-spans
             when (= (nth 0 span) pos)
             return (thread-last (symbol-name (nth 4 span))
                      (string-remove-suffix "-mode")))))

;;; Code-fences callbacks

(defun +nix--multiline-head-valid-p (beg)
  "Return non-nil if BEG is still a valid span head in the cache."
  (cl-loop for span in +nix-ts--cached-spans
           thereis (= (nth 0 span) beg)))

(defun +nix-ts--count-openers ()
  "Return number of cached spans."
  (length +nix-ts--cached-spans))

(provide '+nix-ts-spans)

;;; +nix-ts-spans.el ends here
