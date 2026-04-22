;;; tests.el --- Tests for lang-shscript module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for shell script editing configuration.

;;; Code:

(require 'ert)

(condition-case nil
    (load (expand-file-name "init.el" (file-name-directory (or load-file-name buffer-file-name))))
  (error nil))

;;; P1: Files with nix-shell shebang open in bash-ts-mode

(ert-deftest lang-shscript-test-nix-shell-magic-mode ()
  "Nix-shell shebang should trigger bash-ts-mode."
  (should (assoc-default "#!/usr/bin/env nix-shell\n" magic-mode-alist #'string-match-p)))

;;; P2: sh-set-shell does not emit messages

(ert-deftest lang-shscript-test-sh-set-shell-silenced ()
  "sh-set-shell should have silence advice."
  (should (advice-member-p 'sh-set-shell@silence-messages 'sh-set-shell)))

;;; P3: New .sh files get bash shebang template

(ert-deftest lang-shscript-test-sh-file-template ()
  "Shell script file template should match .sh files."
  (skip-unless (fboundp '+define-file-template))
  (require 'autoinsert)
  (should (cl-some (lambda (entry)
                     (let ((pattern (if (consp (car entry)) (caar entry) (car entry))))
                       (and (stringp pattern)
                            (string-match-p pattern "test.sh"))))
                   auto-insert-alist)))

;;; P4: New .zsh files get zsh shebang template

(ert-deftest lang-shscript-test-zsh-file-template ()
  "Shell script template should match .zsh files."
  (skip-unless (fboundp '+define-file-template))
  (require 'autoinsert)
  (should (cl-some (lambda (entry)
                     (let ((pattern (if (consp (car entry)) (caar entry) (car entry))))
                       (and (stringp pattern)
                            (string-match-p pattern "test.zsh"))))
                   auto-insert-alist)))

;;; P5: Script files made executable on save

(ert-deftest lang-shscript-test-executable-on-save ()
  "after-save-hook should include executable-make-buffer-file-executable-if-script-p."
  (should (memq 'executable-make-buffer-file-executable-if-script-p after-save-hook)))

;;; P6: Tempel snippet dir available

(ert-deftest lang-shscript-test-tempel-dir-snippet ()
  "Tempel snippet 'dir' should exist in sh-base.eld."
  (let ((template-file (expand-file-name "templates/sh-base.eld" user-emacs-directory)))
    (skip-unless (file-exists-p template-file))
    (with-temp-buffer
      (insert-file-contents template-file)
      (should (search-forward "(dir " nil t)))))

;;; P7: Tempel snippet err available

(ert-deftest lang-shscript-test-tempel-err-snippet ()
  "Tempel snippet 'err' should exist in sh-base.eld."
  (let ((template-file (expand-file-name "templates/sh-base.eld" user-emacs-directory)))
    (skip-unless (file-exists-p template-file))
    (with-temp-buffer
      (insert-file-contents template-file)
      (should (search-forward "(err " nil t)))))

;;; argc-mode tests

(load (expand-file-name "lib/+argc.el" (file-name-directory (or load-file-name buffer-file-name))))

(defun argc-test-has-face-p (content face)
  "Return non-nil if FACE appears on any overlay in CONTENT."
  (with-temp-buffer
    (insert content)
    (argc--apply-face-overlays)
    (cl-some (lambda (ov) (eq (overlay-get ov 'face) face))
             (overlays-in (point-min) (point-max)))))

;;; P8: argc-mode is a minor mode

(ert-deftest argc-test-minor-mode-exists ()
  "argc-mode should be defined as a minor mode."
  (should (fboundp 'argc-mode)))

;;; P9: argc-mode creates overlays when enabled

(ert-deftest argc-test-creates-overlays-on-enable ()
  "Enabling argc-mode should create overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'argc))
                     (overlays-in (point-min) (point-max))))))

;;; P10: argc-mode removes overlays when disabled

(ert-deftest argc-test-removes-overlays-on-disable ()
  "Disabling argc-mode should remove all argc overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (argc-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'argc))
                         (overlays-in (point-min) (point-max))))))

;;; P11: @cmd directive fontified

(ert-deftest argc-test-fontify-cmd ()
  "@cmd tag should get argc-directive-face."
  (should (argc-test-has-face-p "# @cmd Upload a file" 'argc-directive-face)))

;;; P12: @describe directive fontified

(ert-deftest argc-test-fontify-describe ()
  "@describe tag should get argc-directive-face."
  (should (argc-test-has-face-p "# @describe A demo cli" 'argc-directive-face)))

;;; P13: @arg name fontified

(ert-deftest argc-test-fontify-arg-name ()
  "@arg parameter name should get argc-param-name-face."
  (should (argc-test-has-face-p "# @arg target! File to upload" 'argc-param-name-face)))

;;; P14: @arg modifier fontified

(ert-deftest argc-test-fontify-arg-modifier ()
  "@arg required modifier ! should get argc-modifier-face."
  (should (argc-test-has-face-p "# @arg target! File to upload" 'argc-modifier-face)))

;;; P15: @option flags fontified

(ert-deftest argc-test-fontify-option-flags ()
  "@option -t --tries should get argc-flag-face."
  (should (argc-test-has-face-p "# @option -t --tries <NUM> Set number" 'argc-flag-face)))

;;; P16: @flag flags fontified

(ert-deftest argc-test-fontify-flag ()
  "@flag -f --force should get argc-flag-face."
  (should (argc-test-has-face-p "# @flag -f --force Override" 'argc-flag-face)))

;;; P17: @env name fontified

(ert-deftest argc-test-fontify-env-name ()
  "@env variable name should get argc-param-name-face."
  (should (argc-test-has-face-p "# @env MY_VAR! Description" 'argc-param-name-face)))

;;; P18: @meta key fontified

(ert-deftest argc-test-fontify-meta-key ()
  "@meta key should get argc-param-name-face."
  (should (argc-test-has-face-p "# @meta version 1.0.0" 'argc-param-name-face)))

;;; P19: @alias names fontified

(ert-deftest argc-test-fontify-alias ()
  "@alias names should get argc-param-name-face."
  (should (argc-test-has-face-p "# @alias u,upload" 'argc-param-name-face)))

;;; P20: Notation <FILE> fontified

(ert-deftest argc-test-fontify-notation ()
  "Angle-bracket notation should get argc-notation-face."
  (should (argc-test-has-face-p "# @option -t --tries <NUM> Set number" 'argc-notation-face)))

;;; P21: Choice list [a|b|c] fontified

(ert-deftest argc-test-fontify-choices ()
  "Choice list should get argc-choice-face."
  (should (argc-test-has-face-p "# @arg val[x|y|z]" 'argc-choice-face)))

;;; P22: Non-argc comments not fontified with argc faces

(ert-deftest argc-test-no-false-positive ()
  "Regular comments should not get argc-directive-face."
  (should-not (argc-test-has-face-p "# This is a regular comment" 'argc-directive-face)))

;;; P23: @option long-only (no short flag)

(ert-deftest argc-test-fontify-option-long-only ()
  "@option --long-only should get argc-flag-face."
  (should (argc-test-has-face-p "# @option --output <FILE> Output file" 'argc-flag-face)))

;;; P24: All directive types recognized

(ert-deftest argc-test-all-directives ()
  "All argc directive types should be fontified."
  (dolist (directive '("@describe" "@cmd" "@alias" "@arg" "@option" "@flag" "@env" "@meta"))
    (should (argc-test-has-face-p
             (format "# %s test" directive)
             'argc-directive-face))))

;;; P25: @arg modifiers *, +, ~

(ert-deftest argc-test-fontify-arg-modifiers ()
  "All @arg modifiers should get argc-modifier-face."
  (dolist (mod '("!" "*" "+" "~"))
    (should (argc-test-has-face-p
             (format "# @arg vals%s desc" mod)
             'argc-modifier-face))))

;;; P26: description text gets font-lock-doc-face

(ert-deftest argc-test-fontify-cmd-description ()
  "@cmd description text should get font-lock-doc-face."
  (should (argc-test-has-face-p "# @cmd Upload a file" 'font-lock-doc-face)))

(ert-deftest argc-test-fontify-arg-description ()
  "@arg description text should get font-lock-doc-face."
  (should (argc-test-has-face-p "# @arg target! File to upload" 'font-lock-doc-face)))

(ert-deftest argc-test-fontify-option-description ()
  "@option description text should get font-lock-doc-face."
  (should (argc-test-has-face-p "# @option -f --force Override existing" 'font-lock-doc-face)))

;;; P27: box overlay — block detection

(ert-deftest argc-test-find-blocks-single ()
  "Single directive line is one block."
  (with-temp-buffer
    (insert "# @cmd Foo\n")
    (should (equal (length (argc--find-blocks)) 1))))

(ert-deftest argc-test-find-blocks-contiguous ()
  "Contiguous directive lines form one block."
  (with-temp-buffer
    (insert "# @cmd Foo\n# @arg bar!\n")
    (let ((blocks (argc--find-blocks)))
      (should (= 1 (length blocks))))))

(ert-deftest argc-test-find-blocks-separated ()
  "Non-contiguous directive lines form separate blocks."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n# @cmd Bar\n")
    (should (= 2 (length (argc--find-blocks))))))

;;; P28: function name detection

(ert-deftest argc-test-function-after-basic ()
  "Detect function name after directive block."
  (with-temp-buffer
    (insert "# @cmd Foo\nmy_func() {\n")
    (should (equal (argc--function-after 11) "my_func"))))

(ert-deftest argc-test-function-after-keyword ()
  "Detect 'function name {' style."
  (with-temp-buffer
    (insert "# @cmd Foo\nfunction my_func {\n")
    (should (equal (argc--function-after 11) "my_func"))))

(ert-deftest argc-test-function-after-none ()
  "Return nil when no function follows."
  (with-temp-buffer
    (insert "# @cmd Foo\necho hello\n")
    (should-not (argc--function-after 11))))

(ert-deftest argc-test-function-after-hyphenated ()
  "Detect hyphenated function names."
  (with-temp-buffer
    (insert "# @cmd Foo\nside-by-side() {\n")
    (should (equal (argc--function-after 11) "side-by-side"))))

;;; P29: box overlays created and removed

(ert-deftest argc-test-overlays-created ()
  "Box overlays should be created for directive blocks."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let ((ovs (cl-remove-if-not
                (lambda (ov) (overlay-get ov 'argc-box))
                (overlays-in (point-min) (point-max)))))
      (should (> (length ovs) 0)))))

(ert-deftest argc-test-overlays-removed ()
  "Box overlays should be removed on cleanup."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (argc--remove-overlays)
    (let ((ovs (cl-remove-if-not
                (lambda (ov) (overlay-get ov 'argc-box))
                (overlays-in (point-min) (point-max)))))
      (should (= 0 (length ovs))))))

;;; P30: box border contains function name

(ert-deftest argc-test-box-header-has-func-name ()
  "Top border overlay should contain the function name."
  (with-temp-buffer
    (insert "# @cmd Foo\nmy_func() {\n}\n")
    (argc--apply-box-overlays)
    (let* ((ovs (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'argc-box))
                 (overlays-in (point-min) (point-max))))
           (before-strings (mapcar (lambda (ov) (overlay-get ov 'before-string)) ovs))
           (has-name (cl-some (lambda (s)
                                (and s (string-match-p "my_func" s)))
                              before-strings)))
      (should has-name))))

;;; P31: box width >= 80

(ert-deftest argc-test-box-min-width-80 ()
  "Top border should be at least 80 characters wide."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let* ((ovs (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'argc-box))
                 (overlays-in (point-min) (point-max))))
           (before-strings (mapcar (lambda (ov) (overlay-get ov 'before-string)) ovs))
           (top-str (cl-find-if (lambda (s) (and s (string-match-p "┌" s))) before-strings)))
      (should top-str)
      (let ((top-line (car (split-string top-str "\n"))))
        (should (>= (string-width top-line) 80))))))

;;; P32: box width expands for long lines

(ert-deftest argc-test-box-expands-for-long-lines ()
  "Box width should expand for lines longer than 76 chars."
  (with-temp-buffer
    (insert (concat "# @cmd " (make-string 80 ?x) "\nfoo() {\n}\n"))
    (argc--apply-box-overlays)
    (let* ((ovs (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'argc-box))
                 (overlays-in (point-min) (point-max))))
           (before-strings (mapcar (lambda (ov) (overlay-get ov 'before-string)) ovs))
           (top-str (cl-find-if (lambda (s) (and s (string-match-p "┌" s))) before-strings)))
      (should top-str)
      (let ((top-line (car (split-string top-str "\n"))))
        (should (> (string-width top-line) 80))))))

;;; P33: +argc-maybe-enable skips if already active

(ert-deftest argc-test-maybe-enable-no-double ()
  "Calling +argc-maybe-enable twice should not double overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (+argc-maybe-enable)
    (let ((count-1 (length (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'argc))
                            (overlays-in (point-min) (point-max))))))
      (+argc-maybe-enable)
      (let ((count-2 (length (cl-remove-if-not
                              (lambda (ov) (overlay-get ov 'argc))
                              (overlays-in (point-min) (point-max))))))
        (should (= count-1 count-2))))))

;;; P34: +argc-maybe-enable skips buffers without directives

(ert-deftest argc-test-maybe-enable-no-directives ()
  "Should not enable argc-mode without argc directives."
  (with-temp-buffer
    (insert "#!/bin/bash\necho hello\n")
    (+argc-maybe-enable)
    (should-not (bound-and-true-p argc-mode))))

;;; P35: +argc-maybe-enable only checks first 50 lines

(ert-deftest argc-test-maybe-enable-beyond-50-lines ()
  "Directives after line 50 should not trigger argc-mode."
  (with-temp-buffer
    (dotimes (_ 55) (insert "echo hello\n"))
    (insert "# @cmd Foo\n")
    (+argc-maybe-enable)
    (should-not (bound-and-true-p argc-mode))))

(ert-deftest argc-test-maybe-enable-within-50-lines ()
  "Directives within first 50 lines should trigger argc-mode."
  (with-temp-buffer
    (dotimes (_ 10) (insert "echo hello\n"))
    (insert "# @cmd Foo\n")
    (+argc-maybe-enable)
    (should (bound-and-true-p argc-mode))))

;;; P35b: +argc-maybe-enable skips indirect buffers

(ert-deftest argc-test-maybe-enable-skip-indirect ()
  "Should not enable argc-mode in indirect buffers."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (let ((indirect (make-indirect-buffer (current-buffer) " *argc-test-indirect*" t)))
      (unwind-protect
          (with-current-buffer indirect
            (+argc-maybe-enable)
            (should-not (bound-and-true-p argc-mode)))
        (kill-buffer indirect)))))

;;; P36: rebuild in indirect buffer is skipped

(ert-deftest argc-test-indirect-buffer-skip ()
  "Indirect buffers should not schedule rebuilds."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (let* ((indirect (make-indirect-buffer (current-buffer) " *argc-test-indirect*" t))
           (scheduled nil))
      (unwind-protect
          (with-current-buffer indirect
            (argc--schedule-rebuild)
            (setq scheduled (timerp argc--rebuild-timer)))
        (kill-buffer indirect))
      (should-not scheduled))))

;;; P37: spell-fu advice skips directive lines

(ert-deftest argc-test-spell-fu-advice-skips-directives ()
  "spell-fu-mark-incorrect should be skipped on directive lines."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (let ((called nil))
      (cl-letf (((symbol-function 'spell-fu-mark-incorrect)
                 (lambda (_beg _end) (setq called t))))
        (argc--spell-fu-skip-directives
         (symbol-function 'spell-fu-mark-incorrect) 2 5)
        (should-not called)))))

(ert-deftest argc-test-spell-fu-advice-allows-non-directives ()
  "spell-fu-mark-incorrect should run on non-directive lines."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (let ((called nil))
      (cl-letf (((symbol-function 'spell-fu-mark-incorrect)
                 (lambda (_beg _end) (setq called t))))
        (argc--spell-fu-skip-directives
         (symbol-function 'spell-fu-mark-incorrect) 12 15)
        (should called)))))

;;; P38: argc-mode 1 is idempotent (no overlay doubling)

(ert-deftest argc-test-mode-enable-idempotent ()
  "Calling (argc-mode 1) when already on should not double overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\n# @arg bar!\nfoo() {\n}\n")
    (argc-mode 1)
    (let ((count-1 (length (cl-remove-if-not
                            (lambda (ov) (overlay-get ov 'argc))
                            (overlays-in (point-min) (point-max))))))
      (argc-mode 1)
      (let ((count-2 (length (cl-remove-if-not
                              (lambda (ov) (overlay-get ov 'argc))
                              (overlays-in (point-min) (point-max))))))
        (should (= count-1 count-2))))))

;;; P39: bottom border overlay exists as separate overlay

(ert-deftest argc-test-bottom-border-separate ()
  "Bottom border should be a zero-width overlay after the block."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let ((bottom-ovs (cl-remove-if-not
                       (lambda (ov)
                         (and (overlay-get ov 'argc-box)
                              (let ((bs (overlay-get ov 'before-string)))
                                (and bs (string-match-p "└" bs)))
                              (= (overlay-start ov) (overlay-end ov))))
                       (overlays-in (point-min) (point-max)))))
      (should (= 1 (length bottom-ovs))))))

;;; P40: no box overlays on non-directive content

(ert-deftest argc-test-no-box-on-regular-comments ()
  "Regular comments should not get box overlays."
  (with-temp-buffer
    (insert "# regular comment\necho hello\n")
    (argc--apply-box-overlays)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'argc-box))
                         (overlays-in (point-min) (point-max))))))

;;; P41: comment continuation lines included in block

(ert-deftest argc-test-find-blocks-comment-continuation ()
  "Plain comment lines between directives should form one block."
  (with-temp-buffer
    (insert "# @describe Open dired in emacsclient.\n# If pane exists, switch.\n#\n# @arg path=. Directory\n")
    (let ((blocks (argc--find-blocks)))
      (should (= 1 (length blocks))))))

(provide 'lang-shscript-tests)

;;; tests.el ends here
