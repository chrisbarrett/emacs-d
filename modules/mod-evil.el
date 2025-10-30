;;; mod-evil.el --- Evil configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'evil)
(require 'general)

;; Cursor customisation
(setq evil-normal-state-cursor 'box)
(setq evil-emacs-state-cursor  'hollow)
(setq evil-insert-state-cursor 'bar)
(setq evil-visual-state-cursor 'hollow)

;; Initial states for modes
(evil-set-initial-state '+bd-issue-mode 'insert)

;; Keybindings for +bd-issue-mode (message history navigation)
(general-def :states '(insert normal) :keymaps '+bd-issue-mode-map
  "M-p" #'+bd-issue-prev-message
  "M-n" #'+bd-issue-next-message)

;; Keep shift-width in sync if mode changes.
(setq-hook! 'after-change-major-mode
  evil-shift-width tab-width)

;; Use more natural Emacs/readline keybindings in ex.
(general-def :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
  "C-a" #'evil-beginning-of-line
  "C-b" #'evil-backward-char
  "C-f" #'evil-forward-char)

;; Define newline behaviour.
;;
;; `comment-indent-new-line' is a nicer default--it inserts comment delimiters
;; for you when you do a newline in a comment. However, it breaks
;; electric-pair's special newline padding functionality, so only call it if
;; we're actually on a comment.

(general-def :states 'insert :keymaps '(prog-mode-map text-mode-map)
  "RET" (general-predicate-dispatch #'newline-and-indent
          (nth 4 (syntax-ppss)) ; at a comment?
          #'comment-indent-new-line))



;; Teach "J" (evil-join) to delete comment delimiters as needed to join
;; lines.

;; Taken from doom, which itself adapts solutions in this github issue:
;; https://github.com/emacs-evil/evil/issues/606

(define-advice evil-join (:around (fn beg end) join-comments)
  (if-let* (((not (= (line-end-position) (point-max))))
            (cend (save-excursion (goto-char end) (line-end-position)))
            (cbeg (save-excursion
                    (goto-char beg)
                    (and (+point-in-comment-p
                          (save-excursion
                            (goto-char (line-beginning-position 2))
                            (skip-syntax-forward " \t")
                            (point)))
                         (or (comment-search-backward (line-beginning-position) t)
                             (comment-search-forward  (line-end-position) t)
                             (and (+point-in-comment-p beg)
                                  (stringp comment-continue)
                                  (or (search-forward comment-continue (line-end-position) t)
                                      beg)))))))
      (let* ((count (count-lines beg end))
             (count (if (> count 1) (1- count) count))
             (fixup-mark (make-marker)))
        (uncomment-region (line-beginning-position 2)
                          (save-excursion
                            (goto-char cend)
                            (line-end-position 0)))
        (unwind-protect
            (dotimes (_ count)
              (join-line 1)
              (save-match-data
                (when (or (and comment-continue
                               (not (string-empty-p comment-continue))
                               (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                          (and comment-start-skip
                               (not (string-empty-p comment-start-skip))
                               (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                  (replace-match "" t nil nil 1)
                  (just-one-space))))
          (set-marker fixup-mark nil)))
    ;; But revert to the default we're not in a comment, where
    ;; `fill-region-as-paragraph' is too greedy.
    (funcall fn beg end)))

;; Teach evil-ret to open links at point.

(define-advice evil-ret (:before-until (&rest _) open-url)
  (when-let* ((url (thing-at-point 'url)))
    (browse-url url)
    t))

(provide 'mod-evil)

;;; mod-evil.el ends here
