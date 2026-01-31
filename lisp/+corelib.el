;; +corelib.el --- General re-usable functions and macros.  -*- lexical-binding: t; -*-

;;; Commentary:

;; A mix of utils copied from Doom, collection utilities, and other things
;; needed to boot the configuration without depending on downloaded 3rd-party
;; packages.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar +config-dir nil)
(defvar +lisp-dir nil)

(defvar +default-minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map))

(defmacro +load (rel-path)
  "Load a file relative to the caller.

REL-PATH is joined with the directory that contains the caller.
The directory is captured at macro-expansion time."
  (let ((this-dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    `(load ,(file-name-concat this-dir rel-path))))


;;; Logging

(defvar +inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `+log' output completely.")

(defvar +log-level
  (if init-file-debug
      (if-let* ((level (getenv-internal "DEBUG"))
                (level (string-to-number level))
                ((not (zerop level))))
          level
        2)
    0)
  "How verbosely to log from `+log' calls.

0 -- No logging at all.
1 -- Only warnings.
2 -- Warnings and notices.
3 -- Debug info, warnings, and notices.")

(defun +log--call (level text &rest args)
  (let ((inhibit-message (if noninteractive
                             (not init-file-debug)
                           (> level +log-level)))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat (lambda (x) (format "%s" x)) ":")
           args)))

;; This is a macro instead of a function to prevent the potentially expensive
;; evaluation of its arguments when debug mode is off. Return non-nil.
(defmacro +log (message &rest args)
  "Log a message to stderr or *Messages* (without displaying in the echo area).

MESSAGE and ARGS are interpreted as in `message'."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message
                     (setq message (pop args)))
                 2)))
    `(when (and (not +inhibit-log)
                (or (not noninteractive)
                    (<= ,level +log-level)))
       (+log--call ,level ,message ,@args))))


;;; setq-hook!

(defun +unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun +corelib--resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (+unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun --setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (+corelib--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "+setq-%s-for-%s-h"
                                          var mode))))))

(defmacro setq-hook! (hooks &rest var-vals)
  "Set buffer-local variables on HOOKS.

HOOKS is a symbol or list of symbols representing hook names. See
`add-hook!' for more details.


VAR-VALS are alternating pairs of variable names and values to set.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (--setq-hook-fns hooks var-vals)
            collect `(eval-and-compile
                       (defun ,fn (&rest _)
                         (setq-local ,var ,val)))
            collect `(add-hook ',hook #',fn -90))))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "+transient-hook")))
    `(let ((sym ,hook-or-function))
       (eval-and-compile
         (defun ,fn (&rest _)
           ,(format "Transient hook for %S" (+unquote hook-or-function))
           ,@forms
           (let ((sym ,hook-or-function))
             (cond ((functionp sym) (advice-remove sym #',fn))
                   ((symbolp sym)   (remove-hook sym #',fn))))
           (unintern ',fn nil)))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))


;;; Hooks

(defvar +corelib--this-hook nil)

(defun +run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (+log "hook:%s: run %s" (or +corelib--this-hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal '+corelib-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun +run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((+corelib--this-hook hook))
          (run-hook-wrapped hook #'+run-hook))
      (+corelib-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal '+corelib-hook-error (cons hook (cdr e)))))))

(defun +run-hook-once (hook-var trigger-hooks)
  "Configure a hook to run after any of another set of hooks, but only once.

HOOK-VAR will be invoked exactly once when any of the TRIGGER-HOOKS are
invoked *after* Emacs has initialized (to reduce false positives).

HOOK-VAR is a quoted hook. Once HOOK-VAR is triggered, it is reset to
nil.

TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and (not running?)
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In that case assume this hook was
                           ;; invoked non-interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)  ; prevent infinite recursion
              (+run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        ;; In a daemon session we don't need all these lazy loading shenanigans.
        ;; Just load everything immediately.
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          ;; Advise `after-find-file' instead of using `find-file-hook' because
          ;; the latter is triggered too late (after the file has opened and
          ;; modes are all set up).
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (+corelib--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))



(defun +visible-buffers (&optional buffer-list all-frames)
  "Return a list of visible buffers (i.e. not buried).

BUFFER-LIST is a list of buffers to inspect; the global buffer list is
used if unset.

Returns buffers visible in the selected frame, unless ALL-FRAMES is set."
  (let ((buffers
	 (delete-dups
	  (cl-loop for frame in (if all-frames (visible-frame-list) (list (selected-frame)))
		   if (window-list frame)
		   nconc (mapcar #'window-buffer it)))))
    (if buffer-list
	(cl-loop for buf in buffers
		 unless (memq buf buffer-list)
		 collect buffers)
      buffers)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

(defun +separate (pred sequence)
  "Partition SEQUENCE by applying PRED to each element.
Elements are partitioned according to whether the result is truthy or
nil.

Return a cons cell, where the CAR is the truthy partition and the CDR is
the falsey partition."
  (let (truthy falsey)
    (seq-do (lambda (it)
              (push it (if (funcall pred it)
                           truthy
                         falsey)))
            sequence)
    (cons truthy falsey)))

(defun +split-with (pred sequence)
  "Split SEQUENCE into two according to PRED.

Returns two lists; the first contains every item up until PRED returns
nil. The second list contains that element and all subsequent elements."
  (let ((continue t)
        (prefix))
    (while (and sequence continue)
      (let ((item (car sequence)))
        (cond
         ((funcall pred item)
          (push item prefix)
          (pop sequence))
         (t
          (setq continue nil)))))

    (list (nreverse prefix) sequence)))

(defun +chunk-by (pred sequence)
  "Split SEQUENCE into separate chunks according to PRED.

Every time PRED returns non-nil, the list is split into a new chunk."

  ;; (+chunk-by #'numberp ())
  ;; (+chunk-by #'numberp '(1))
  ;; (+chunk-by #'numberp '(1 2))
  ;; (+chunk-by #'numberp '(1 a 2 b))
  ;; (+chunk-by #'numberp '(a 1 b 2))
  ;; (+chunk-by #'numberp '(1 a a 2 b))
  ;; (+chunk-by #'numberp '(a a 1 2 b 3))

  (cl-labels ((loop (unprocessed acc current-chunk)
                (pcase-exhaustive (cons unprocessed current-chunk)
                  (`(() . ())
                   (nreverse acc))

                  (`(() . ,current-chunk)
                   (nreverse (cons (nreverse current-chunk) acc)))

                  (`((,h . ,tl) . ())
                   (loop tl nil (list h)))

                  (`((,h . ,tl) . ,current-chunk)
                   (if (funcall pred h)
                       (loop tl
                             (cons (nreverse current-chunk) acc)
                             (list h))
                     (loop tl
                           acc
                           (cons h current-chunk)))))))
    (loop sequence nil nil)))

(defun +alist-from-hash-table (hash-table)
  "Convert HASH-TABLE into an alist."

  ;; (+alist-from-hash-table (let ((ht (make-hash-table)))
  ;;                           (puthash :foo t ht)
  ;;                           (puthash :bar t ht)
  ;;                           ht))

  (let ((result))
    (dolist (key (hash-table-keys hash-table))
      (push (cons key (gethash key hash-table))
            result))
    (nreverse result)))

(defun +plist-from-hash-table (hash-table)
  "Convert HASH-TABLE into an plist."

  ;; (+plist-from-hash-table (let ((ht (make-hash-table)))
  ;;                           (puthash :foo 'foo ht)
  ;;                           (puthash :bar 'bar ht)
  ;;                           ht))

  (let ((result))
    (dolist (key (hash-table-keys hash-table))
      (setq result (cons (gethash key hash-table) (cons key result))))
    (nreverse result)))

(defun +tree-map (fn tree)
  "Perform a pre-order traversal of TREE using FN."
  (let ((current (funcall fn tree)))
    (pcase current
      (`(,l . ,r)
       (cons (+tree-map fn l) (+tree-map fn r)))
      (it
          it))))

(defmacro alist-set! (alist key value)
  `(setf (alist-get ,key ,alist nil nil #'equal) ,value))

(defun +read-eld (file)
  "Read Lisp data FILE into memory."
  (with-temp-buffer
    (insert-file-contents-literally (file-name-concat user-emacs-directory file))
    (read (current-buffer))))

(defun +plist-delete (key plist)
  (let (result)
    (while plist
      (unless (equal (car plist) key)
        (push (car plist) result)
        (push (cadr plist) result))
      (setq plist (cddr plist)))
    (nreverse result)))



(defvar-local +sppss-memo-last-point nil)
(defvar-local +sppss-memo-last-result nil)

(defun +sppss-memo-reset-h (&rest _ignored)
  "Reset memoization as a safety precaution.

IGNORED is a dummy argument used to eat up arguments passed from
the hook where this is executed."
  (setq +sppss-memo-last-point nil
        +sppss-memo-last-result nil))

(defun +syntax-ppss (&optional p)
  "Memoize the last result of `syntax-ppss'.

P is the point at which we run `syntax-ppss'"
  (let ((p (or p (point)))
        (mem-p +sppss-memo-last-point))
    (if (and (eq p (nth 0 mem-p))
             (eq (point-min) (nth 1 mem-p))
             (eq (point-max) (nth 2 mem-p)))
        +sppss-memo-last-result
      ;; Add hook to reset memoization if necessary
      (unless +sppss-memo-last-point
        (add-hook 'before-change-functions #'+sppss-memo-reset-h t t))
      (setq +sppss-memo-last-point (list p (point-min) (point-max))
            +sppss-memo-last-result (syntax-ppss p)))))

(defun +point-in-comment-p (&optional pt)
  (let ((pt (or pt (point))))
    (ignore-errors
      (save-excursion
        ;; We cannot be in a comment if we are inside a string
        (unless (nth 3 (+syntax-ppss pt))
          (or (nth 4 (+syntax-ppss pt))
              ;; this also test opening and closing comment delimiters... we
              ;; need to chack that it is not newline, which is in "comment
              ;; ender" class in elisp-mode, but we just want it to be treated
              ;; as whitespace
              (and (< pt (point-max))
                   (memq (char-syntax (char-after pt)) '(?< ?>))
                   (not (eq (char-after pt) ?\n)))
              ;; we also need to test the special syntax flag for comment
              ;; starters and enders, because `syntax-ppss' does not yet know if
              ;; we are inside a comment or not (e.g. / can be a division or
              ;; comment starter...).
              (when-let* ((s (car (syntax-after pt))))
                (or (and (/= 0 (logand (ash 1 16) s))
                         (nth 4 (syntax-ppss (+ pt 2))))
                    (and (/= 0 (logand (ash 1 17) s))
                         (nth 4 (syntax-ppss (+ pt 1))))
                    (and (/= 0 (logand (ash 1 18) s))
                         (nth 4 (syntax-ppss (- pt 1))))
                    (and (/= 0 (logand (ash 1 19) s))
                         (nth 4 (syntax-ppss (- pt 2))))))))))))

(defmacro +local-leader-set-key (keymaps &rest general-args)
  (declare (indent 1))
  `(progn
     (cl-eval-when (compile)
       (autoload 'general-define-key "general"))
     (general-define-key :prefix "," :states '(normal motion) :keymaps ,keymaps ,@general-args)))



(defvar +dirlocals--path-patterns-alist nil
  "An alist of path patterns (regexps) to directory variables.")

(defun +dirlocals-set (prefix-or-prefixes variables)
  "Set directory variables based on a path prefix.

PREFIX-OR-PREFIXES is an absolute path (a string) or list thereof.
VARIABLES will be applied to any file beginning with one of these."
  (declare (indent 1))
  (+dirlocals-set-regexp (seq-map (lambda (path)
				    (rx-to-string `(seq bos ,(expand-file-name (if (string-suffix-p "/" path) ; must end with /
								                   path
                                                                                 (concat path "/"))))
						  t))
				  (ensure-list prefix-or-prefixes))
    variables))

(defun +dirlocals-set-regexp (regexp-or-regexps variables)
  "Set directory variables based on a regular expression.

REGEXP-OR-REGEXPS is a regular expression (a string) or list thereof.
VARIABLES will be applied to any matching file."
  (declare (indent 1))
  (dolist (pat (ensure-list regexp-or-regexps))
    (alist-set! +dirlocals--path-patterns-alist pat variables)))

(defun +dirlocals--specs-for-path (&optional path)
  "Return pattern-based directory variables.

  Return variables for the current file by default. If PATH is non-nil,
  return variables for PATH instead.

  Used in `hack-dir-local-get-variables-functions' to apply directory
  variables to files based on a regular expression match."

  ;; Expected output structure is one of, or a list of:
  ;;
  ;; ("/Users/chris/.config/emacs/init/" .
  ;;   ((elisp-flymake-byte-compile-load-path . ("./" "~/.config/emacs/lisp"))
  ;;    (mode . elpaca-packages-load-path)))

  (when (and enable-local-variables enable-dir-local-variables)
    (let ((path (or path (buffer-file-name) default-directory)))
      (seq-keep (pcase-lambda (`(,path-pat . ,settings))
                  (save-match-data
                    (when (string-match path-pat path)
                      (let ((matched-prefix (substring path 0 (match-end 0))))
                        (cons matched-prefix
                              (dir-locals-collect-variables settings
                                                            path
                                                            nil))))))
                +dirlocals--path-patterns-alist))))

(add-hook 'hack-dir-local-get-variables-functions #'+dirlocals--specs-for-path)

(provide '+corelib)

;;; +corelib.el ends here

;; Local Variables:
;; checkdoc-arguments-missing-flag: nil
;; End:
