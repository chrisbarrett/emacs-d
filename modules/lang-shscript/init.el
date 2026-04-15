;;; init.el --- Shell script configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Shell script editing with Tree-sitter, auto-executable, file templates,
;; and polymode for syntax-highlighted heredocs.

;;; Code:

(require '+autoloads)
(require 'cl-lib)

(add-to-list 'magic-mode-alist `(,(rx bol "#!" (+? nonl) "nix-shell" eol) . bash-ts-mode))

(define-advice sh-set-shell (:around (fn &rest args) silence-messages)
  (cl-letf (((symbol-function 'message) #'ignore))
    (apply fn args)))

(+define-file-template (rx "." (or "sh" "bash" "zsh") eos) "shell-script.eld")

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;;; Polymode — syntax highlighting for heredocs
;;
;; Matches all heredoc quoting styles:
;;   <<DELIM  <<'DELIM'  <<"DELIM"  <<\DELIM  <<-DELIM  etc.

(use-package polymode
  :defer t
  :init
  (defun +bash-poly--heredoc-head-matcher (delim)
    "Return head-matcher regex for bash heredoc with DELIM.
Handles <<, <<-, and quoting with single quotes, double quotes, or backslash."
    (rx-to-string
     `(seq "<<" (? "-")
           (or (seq "'" ,delim "'")
               (seq "\"" ,delim "\"")
               (seq "\\" ,delim)
               ,delim)
           eol)))

  (defun +bash-poly--heredoc-tail-matcher (delim)
    "Return tail-matcher regex for bash heredoc with DELIM."
    (rx-to-string `(seq bol ,delim eol)))

  (define-hostmode poly-bash-ts-hostmode
    :mode 'bash-ts-mode)

  (define-innermode poly-bash-elisp-innermode
    :mode 'emacs-lisp-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "ELISP")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "ELISP")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-python-innermode
    :mode 'python-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "PYTHON")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "PYTHON")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-ruby-innermode
    :mode 'ruby-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "RUBY")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "RUBY")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-sql-innermode
    :mode 'sql-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "SQL")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "SQL")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-json-innermode
    :mode 'json-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "JSON")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "JSON")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-yaml-innermode
    :mode 'yaml-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "YAML")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "YAML")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-html-innermode
    :mode 'html-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "HTML")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "HTML")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-css-innermode
    :mode 'css-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "CSS")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "CSS")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-js-innermode
    :mode 'js-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "JS")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "JS")
    :head-mode 'host
    :tail-mode 'host)

  (define-innermode poly-bash-nix-innermode
    :mode 'nix-ts-mode
    :head-matcher (+bash-poly--heredoc-head-matcher "NIX")
    :tail-matcher (+bash-poly--heredoc-tail-matcher "NIX")
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-bash-ts-mode
    :hostmode 'poly-bash-ts-hostmode
    :innermodes '(poly-bash-elisp-innermode
                  poly-bash-python-innermode
                  poly-bash-ruby-innermode
                  poly-bash-sql-innermode
                  poly-bash-json-innermode
                  poly-bash-yaml-innermode
                  poly-bash-html-innermode
                  poly-bash-css-innermode
                  poly-bash-js-innermode
                  poly-bash-nix-innermode))

  (add-to-list 'major-mode-remap-alist '(sh-mode . poly-bash-ts-mode))
  (add-to-list 'major-mode-remap-alist '(bash-ts-mode . poly-bash-ts-mode))

  (add-hook 'poly-bash-ts-mode-hook #'+polymode-refontify-inner-spans))

;;; Code fences — shell-specific dispatch callbacks

(defun +bash--heredoc-unquoted-p (head-beg _head-end)
  "Return non-nil if heredoc head at HEAD-BEG is unquoted.
Unquoted heredocs (<<DELIM, <<-DELIM) get shell expansion.
Quoted heredocs (<<'DELIM', <<\"DELIM\", <<\\DELIM) do not."
  (save-excursion
    (goto-char head-beg)
    (and (looking-at "<<-?\\([^[:space:]]\\)")
         (let ((ch (char-after (match-beginning 1))))
           (not (memq ch '(?' ?\" ?\\)))))))

(defun +bash--add-interpolation-overlays (beg end base)
  "Add overlays for shell interpolation patterns between BEG and END.
Patterns: $VAR, ${...}, $(...), $((...)), `...`.
Skips escaped \\$ sequences.  Overlays are created in BASE buffer."
  (with-current-buffer base
    (save-excursion
      (goto-char beg)
      (while (re-search-forward
              (rx (or
                   (seq (group "$")
                        (or (seq (any "A-Za-z_") (* (any "A-Za-z_0-9")))
                            (seq "{" (*? anything) "}")
                            (seq "((" (*? anything) "))")
                            (seq "(" (*? anything) ")")))
                   (group "`" (+? anything) "`")))
              end t)
        (let* ((mbeg (or (match-beginning 1) (match-beginning 2)))
               (mend (match-end 0)))
          (let ((bs 0))
            (when (> mbeg beg)
              (save-excursion
                (goto-char mbeg)
                (while (eq (char-before (- (point) bs)) ?\\)
                  (cl-incf bs))))
            (when (cl-evenp bs)
              (let ((ov (make-overlay mbeg mend base)))
                (overlay-put ov 'face '+polymode-interpolation-face)
                (overlay-put ov '+polymode-fence t)))))))))

(defconst +bash--heredoc-opener-re "<<-?[\\\\'\"]?[A-Za-z_]"
  "Regexp matching a heredoc opener.")

(defun +bash--count-heredoc-openers ()
  "Count heredoc openers in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward +bash--heredoc-opener-re nil t)
        (cl-incf count))
      count)))

(defun +bash--heredoc-head-valid-p (beg)
  "Return non-nil if overlay at BEG still points at a heredoc opener."
  (save-excursion
    (goto-char beg)
    (looking-at +bash--heredoc-opener-re)))

(with-eval-after-load '+code-fences
  (+code-fences-register 'bash-ts-mode
                         :head-valid-p #'+bash--heredoc-head-valid-p
                         :count-openers #'+bash--count-heredoc-openers
                         :unquoted-p #'+bash--heredoc-unquoted-p
                         :interpolation-fn #'+bash--add-interpolation-overlays))

;;; argc-mode — fontify argc directives

(defun +argc-maybe-enable ()
  "Enable `argc-mode' if an argc directive appears in the first 50 lines."
  (unless (or (bound-and-true-p argc-mode)
              (buffer-base-buffer))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((limit (save-excursion (forward-line 50) (point))))
          (when (re-search-forward
                 (rx bol (* space) "#" (+ space)
                     "@" (or "describe" "cmd" "alias" "arg" "option"
                             "flag" "env" "meta"))
                 limit t)
            (argc-mode 1)))))))

(add-hook 'sh-mode-hook #'+argc-maybe-enable)
(add-hook 'bash-ts-mode-hook #'+argc-maybe-enable)
(add-hook 'poly-bash-ts-mode-hook #'+argc-maybe-enable)

;;; Separedit — heredoc language detection for bash-ts-mode

(with-eval-after-load 'separedit
  (add-to-list 'separedit-heredoc-language-regexp-alist
               '(bash-ts-mode . "<<-? *[\\\\'\"]?_*\\([[:alnum:]]+\\)_*[\"']?\\(?:.*\\)?"))
  (dolist (entry '(("json" . json-ts-mode)
                   ("yaml" . yaml-ts-mode)
                   ("nix" . nix-ts-mode)
                   ("python" . python-ts-mode)
                   ("ruby" . ruby-ts-mode)
                   ("bash" . bash-ts-mode)
                   ("js" . js-ts-mode)
                   ("css" . css-ts-mode)))
    (add-to-list 'separedit-code-lang-modes entry)))

;;; init.el ends here
