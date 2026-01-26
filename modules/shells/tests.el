;;; tests.el --- Tests for shells module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for shell and terminal emulator configuration.
;; Tests P1-P9 from spec.md.

;;; Code:

(require 'ert)

;; Capture module directory at load-time
(defvar shells--test-dir
  (file-name-directory (or load-file-name buffer-file-name)))

;; Load module init
(condition-case nil
    (load (expand-file-name "init.el" shells--test-dir))
  (error nil))

;; Load lib for testing
(condition-case nil
    (load (expand-file-name "lib.el" shells--test-dir))
  (error nil))


;;; P1: eat-term-name is "xterm-256color"

(ert-deftest shells/p1-eat-term-name ()
  "P1: eat-term-name is xterm-256color."
  (skip-unless (featurep 'eat))
  (should (equal eat-term-name "xterm-256color")))


;;; P2: eat-kill-buffer-on-exit is t

(ert-deftest shells/p2-eat-kill-buffer-on-exit ()
  "P2: eat-kill-buffer-on-exit is t."
  (skip-unless (featurep 'eat))
  (should (eq eat-kill-buffer-on-exit t)))


;;; P3: s-v in eat-semi-char-mode-map binds to eat-yank

(ert-deftest shells/p3-eat-yank-binding ()
  "P3: s-v in eat-semi-char-mode-map binds to eat-yank."
  (skip-unless (featurep 'eat))
  (skip-unless (boundp 'eat-semi-char-mode-map))
  (should (eq (lookup-key eat-semi-char-mode-map (kbd "s-v")) 'eat-yank)))


;;; P4: +zoxide-query calls zoxide binary

(ert-deftest shells/p4-zoxide-query-defined ()
  "P4: +zoxide-query function is defined."
  (should (fboundp '+zoxide-query)))


;;; P5: eshell/j changes directory to zoxide result

(ert-deftest shells/p5-eshell-j-defined ()
  "P5: eshell/j command is defined."
  (should (fboundp 'eshell/j)))


;;; P6: eshell/cd advice updates zoxide database

(ert-deftest shells/p6-eshell-cd-advice ()
  "P6: eshell/cd has zoxide update advice."
  ;; Load em-dirs to ensure advice is applied
  (require 'em-dirs)
  (should (advice-member-p 'eshell/cd@update-zoxide 'eshell/cd)))


;;; P7: eshell/g finds git root

(ert-deftest shells/p7-eshell-g-defined ()
  "P7: eshell/g command is defined."
  (should (fboundp 'eshell/g)))

(ert-deftest shells/p7-eshell-g-uses-locate-dominating-file ()
  "P7: eshell/g uses locate-dominating-file for .git."
  (let ((source (symbol-function 'eshell/g)))
    (should (string-match-p "locate-dominating-file"
                            (prin1-to-string source)))))


;;; P8: Evil is disabled in eat buffers

(ert-deftest shells/p8-evil-buffer-regexps ()
  "P8: Evil buffer regexps include eat pattern."
  (skip-unless (featurep 'eat))
  (skip-unless (featurep 'evil))
  (should (cl-some (lambda (entry)
                     (string-match-p "\\*eat" (car entry)))
                   evil-buffer-regexps)))


;;; P9: +evil-collection-disabled-list includes eat

(ert-deftest shells/p9-evil-collection-disabled ()
  "P9: +evil-collection-disabled-list includes eat."
  (skip-unless (featurep '+evil-collection))
  (should (memq 'eat +evil-collection-disabled-list)))


;;; Additional tests

(ert-deftest shells/zoxide-add-defined ()
  "+zoxide-add function is defined."
  (should (fboundp '+zoxide-add)))

(ert-deftest shells/suppress-variable-defined ()
  "+eshell-suppress-zoxide-updates-p variable is defined."
  (should (boundp '+eshell-suppress-zoxide-updates-p)))

(ert-deftest shells/eat-scroll-hook ()
  "eat-mode-hook includes scroll prevention."
  (skip-unless (featurep 'eat))
  (should (cl-some (lambda (fn)
                     (and (symbolp fn)
                          (string-match-p "overscroll" (symbol-name fn))))
                   eat-mode-hook)))

(ert-deftest shells/eat-passthrough-keys ()
  "Passthrough keys bind to eat-self-input."
  (skip-unless (featurep 'eat))
  (skip-unless (boundp 'eat-semi-char-mode-map))
  (should (eq (lookup-key eat-semi-char-mode-map (kbd "C-u")) 'eat-self-input))
  (should (eq (lookup-key eat-semi-char-mode-map (kbd "C-o")) 'eat-self-input))
  (should (eq (lookup-key eat-semi-char-mode-map [escape]) 'eat-self-input)))

(ert-deftest shells/eat-intercepted-keys ()
  "Intercepted keys are nil."
  (skip-unless (featurep 'eat))
  (skip-unless (boundp 'eat-semi-char-mode-map))
  (skip-unless (featurep 'general))
  ;; After general sets them to nil, they should be nil or undefined
  (should-not (lookup-key eat-semi-char-mode-map (kbd "M-B"))))

(ert-deftest shells/module-structure ()
  "Module has required files."
  (should (file-exists-p (expand-file-name "packages.eld" shells--test-dir)))
  (should (file-exists-p (expand-file-name "lib.el" shells--test-dir)))
  (should (file-exists-p (expand-file-name "init.el" shells--test-dir)))
  (should (file-exists-p (expand-file-name "spec.md" shells--test-dir))))

(provide 'shells-tests)

;;; tests.el ends here
