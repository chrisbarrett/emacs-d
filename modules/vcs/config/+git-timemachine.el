;;; +git-timemachine.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'git-timemachine)

;; git-timemachine uses `delay-mode-hooks', which can suppress font-lock.

(add-hook 'git-timemachine-mode-hook #'font-lock-mode)

;; Show information in header-line for better visibility.

(define-advice git-timemachine--show-minibuffer-details (:override (revision) use-header-line)
  "Show revision details in the header-line, instead of the minibuffer."
  (let* ((date-relative (nth 3 revision))
         (date-full (nth 4 revision))
         (author (if git-timemachine-show-author (concat (nth 6 revision) ": ") ""))
         (sha-or-subject (if (eq git-timemachine-minibuffer-detail 'commit) (car revision) (nth 5 revision))))
    (setq header-line-format
          (format "%s%s [%s (%s)]"
                  (propertize author 'face 'git-timemachine-minibuffer-author-face)
                  (propertize sha-or-subject 'face 'git-timemachine-minibuffer-detail-face)
                  date-full date-relative))))

;; Ensure evil keymaps are applied
(with-eval-after-load 'evil
  (add-hook 'git-timemachine-mode-hook 'evil-normalize-keymaps))

;;; +git-timemachine.el ends here
