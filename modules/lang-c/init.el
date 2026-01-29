;;; lang-c/init.el --- C language support -*- lexical-binding: t; -*-

;;; Commentary:

;; C development with Tree-sitter mode and smart editing.
;; - Tree-sitter syntax via c-ts-mode (remapped from c-mode)
;; - Electric angle bracket on #include lines
;; - Auto-semicolon newline with S-RET

;;; Code:

(require '+autoloads)

(require '+corelib)

(defun +c-auto-insert-semi-newline ()
  "Insert semicolon at EOL if needed, then newline and indent.
If line ends with `{', `:', or `;', insert only newline.
Otherwise insert semicolon first."
  (interactive)
  (goto-char (line-end-position))
  (unless (thing-at-point-looking-at (rx (any "{:;") (* space) eol))
    (insert ";"))
  (evil-insert-state)
  (newline-and-indent))

(defun +c-electric-left-angle-bracket (&optional _arg)
  "Insert `<>' pair on #include lines, otherwise normal `<'.
Normalizes spacing before the angle bracket on include lines."
  (interactive "P")
  (let* ((current-line (buffer-substring (line-beginning-position) (line-end-position)))
         (include-line-p (string-match-p (rx bol (* space) "#" (* space) "include" symbol-end)
                                         current-line)))
    (cond (include-line-p
           (just-one-space)
           (insert "<")
           (save-excursion
             (insert ">")))
          (t
           (call-interactively #'self-insert-command)))))

(use-package c-ts-mode
  :general-config
  (:keymaps 'c-ts-mode-map :states 'insert
            "<" #'+c-electric-left-angle-bracket)

  (:keymaps 'c-ts-mode-map :states '(normal insert)
            "S-RET" #'+c-auto-insert-semi-newline))

(alist-set! major-mode-remap-alist #'c-mode #'c-ts-mode)


