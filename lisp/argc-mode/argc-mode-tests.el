;;; argc-mode-tests.el --- Tests for argc-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for `argc-mode' library behaviour: face cascade, box
;; overlays, block detection, mode toggling, spell-fu advice, and the
;; debounced rebuild scheduler.
;;
;; Tests for the `+argc-maybe-enable' activation gate live with the
;; `lang-shscript' module that owns the function.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'argc-mode)

(defun argc-test-has-face-p (content face)
  "Return non-nil if FACE appears on any overlay in CONTENT."
  (with-temp-buffer
    (insert content)
    (argc--apply-face-overlays)
    (cl-some (lambda (ov) (eq (overlay-get ov 'face) face))
             (overlays-in (point-min) (point-max)))))


;;; Autoload + feature wiring

(ert-deftest argc-mode/autoloaded-without-require ()
  "`argc-mode' SHALL be reachable via the harvested autoload entry."
  (should (fboundp 'argc-mode)))

(ert-deftest argc-mode/feature-provided-after-require ()
  "`(require 'argc-mode)' SHALL provide the `argc-mode' feature."
  (should (require 'argc-mode))
  (should (featurep 'argc-mode)))


;;; Minor-mode lifecycle

(ert-deftest argc-test-minor-mode-exists ()
  "argc-mode should be defined as a minor mode."
  (should (fboundp 'argc-mode)))

(ert-deftest argc-test-creates-overlays-on-enable ()
  "Enabling argc-mode should create overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'argc))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest argc-test-removes-overlays-on-disable ()
  "Disabling argc-mode should remove all argc overlays."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (argc-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'argc))
                         (overlays-in (point-min) (point-max))))))


;;; Face cascade

(ert-deftest argc-test-fontify-cmd ()
  "@cmd tag should get argc-directive-face."
  (should (argc-test-has-face-p "# @cmd Upload a file" 'argc-directive-face)))

(ert-deftest argc-test-fontify-describe ()
  "@describe tag should get argc-directive-face."
  (should (argc-test-has-face-p "# @describe A demo cli" 'argc-directive-face)))

(ert-deftest argc-test-fontify-arg-name ()
  "@arg parameter name should get argc-param-name-face."
  (should (argc-test-has-face-p "# @arg target! File to upload" 'argc-param-name-face)))

(ert-deftest argc-test-fontify-arg-modifier ()
  "@arg required modifier ! should get argc-modifier-face."
  (should (argc-test-has-face-p "# @arg target! File to upload" 'argc-modifier-face)))

(ert-deftest argc-test-fontify-option-flags ()
  "@option -t --tries should get argc-flag-face."
  (should (argc-test-has-face-p "# @option -t --tries <NUM> Set number" 'argc-flag-face)))

(ert-deftest argc-test-fontify-flag ()
  "@flag -f --force should get argc-flag-face."
  (should (argc-test-has-face-p "# @flag -f --force Override" 'argc-flag-face)))

(ert-deftest argc-test-fontify-env-name ()
  "@env variable name should get argc-param-name-face."
  (should (argc-test-has-face-p "# @env MY_VAR! Description" 'argc-param-name-face)))

(ert-deftest argc-test-fontify-meta-key ()
  "@meta key should get argc-param-name-face."
  (should (argc-test-has-face-p "# @meta version 1.0.0" 'argc-param-name-face)))

(ert-deftest argc-test-fontify-alias ()
  "@alias names should get argc-param-name-face."
  (should (argc-test-has-face-p "# @alias u,upload" 'argc-param-name-face)))

(ert-deftest argc-test-fontify-notation ()
  "Angle-bracket notation should get argc-notation-face."
  (should (argc-test-has-face-p "# @option -t --tries <NUM> Set number" 'argc-notation-face)))

(ert-deftest argc-test-fontify-choices ()
  "Choice list should get argc-choice-face."
  (should (argc-test-has-face-p "# @arg val[x|y|z]" 'argc-choice-face)))

(ert-deftest argc-test-no-false-positive ()
  "Regular comments should not get argc-directive-face."
  (should-not (argc-test-has-face-p "# This is a regular comment" 'argc-directive-face)))

(ert-deftest argc-test-fontify-option-long-only ()
  "@option --long-only should get argc-flag-face."
  (should (argc-test-has-face-p "# @option --output <FILE> Output file" 'argc-flag-face)))

(ert-deftest argc-test-all-directives ()
  "All argc directive types should be fontified."
  (dolist (directive '("@describe" "@cmd" "@alias" "@arg" "@option" "@flag" "@env" "@meta"))
    (should (argc-test-has-face-p
             (format "# %s test" directive)
             'argc-directive-face))))

(ert-deftest argc-test-fontify-arg-modifiers ()
  "All @arg modifiers should get argc-modifier-face."
  (dolist (mod '("!" "*" "+" "~"))
    (should (argc-test-has-face-p
             (format "# @arg vals%s desc" mod)
             'argc-modifier-face))))

(ert-deftest argc-test-fontify-cmd-description ()
  "@cmd description text should get font-lock-doc-face."
  (should (argc-test-has-face-p "# @cmd Upload a file" 'font-lock-doc-face)))

(ert-deftest argc-test-fontify-arg-description ()
  "@arg description text should get font-lock-doc-face."
  (should (argc-test-has-face-p "# @arg target! File to upload" 'font-lock-doc-face)))

(ert-deftest argc-test-fontify-option-description ()
  "@option description text should get font-lock-doc-face."
  (should (argc-test-has-face-p "# @option -f --force Override existing" 'font-lock-doc-face)))


;;; Block detection

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


;;; Function-name detection

(ert-deftest argc-test-function-after-basic ()
  "Detect function name after directive block."
  (with-temp-buffer
    (insert "# @cmd Foo\nmy_func() {\n")
    (should (equal (argc--function-after 11) "my_func"))))

(ert-deftest argc-test-function-after-keyword ()
  "Detect `function name {' style."
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


;;; Box overlays

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


;;; Rebuild scheduler

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


;;; spell-fu advice

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


;;; Idempotence

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


;;; Box border placement

(ert-deftest argc-test-bottom-border-on-last-line ()
  "Bottom border should be in the last line overlay's after-string."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let ((has-bottom (cl-some
                       (lambda (ov)
                         (and (overlay-get ov 'argc-box)
                              (let ((as (overlay-get ov 'after-string)))
                                (and as (string-match-p "└" as)))))
                       (overlays-in (point-min) (point-max)))))
      (should has-bottom))))

(ert-deftest argc-test-no-box-on-regular-comments ()
  "Regular comments should not get box overlays."
  (with-temp-buffer
    (insert "# regular comment\necho hello\n")
    (argc--apply-box-overlays)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'argc-box))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest argc-test-find-blocks-comment-continuation ()
  "Plain comment lines between directives should form one block."
  (with-temp-buffer
    (insert "# @describe Open dired in emacsclient.\n# If pane exists, switch.\n#\n# @arg path=. Directory\n")
    (let ((blocks (argc--find-blocks)))
      (should (= 1 (length blocks))))))

(ert-deftest argc-test-no-zero-width-box-overlay ()
  "Bottom border should not use a zero-width overlay at a navigable position."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (should-not (cl-some (lambda (ov)
                           (and (overlay-get ov 'argc-box)
                                (= (overlay-start ov) (overlay-end ov))))
                         (overlays-in (point-min) (point-max))))))

(provide 'argc-mode-tests)

;;; argc-mode-tests.el ends here
