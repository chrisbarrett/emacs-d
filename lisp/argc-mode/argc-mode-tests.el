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

(defun argc-test--collect-display-aligns (s)
  "Walk S; return all :align-to values from `space' display specs."
  (let ((pos 0)
        (len (length s))
        out)
    (while (< pos len)
      (let* ((disp (get-text-property pos 'display s))
             (next (or (next-single-property-change pos 'display s) len)))
        (when (and (consp disp) (eq (car disp) 'space))
          (let ((a (plist-get (cdr disp) :align-to)))
            (when a (push a out))))
        (setq pos next)))
    (nreverse out)))

(defun argc-test--aligns-to-right-p (a)
  "Return non-nil if A is `right' or `(- right N)'."
  (or (eq a 'right)
      (and (consp a) (eq (car a) '-) (eq (cadr a) 'right))))

(ert-deftest argc-test-right-border-aligns-to-window-edge ()
  "Per-line right-edge after-string SHALL position via `:align-to right'."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let* ((ovs (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'argc-box))
                 (overlays-in (point-min) (point-max))))
           (after-strings (delq nil (mapcar (lambda (ov) (overlay-get ov 'after-string)) ovs)))
           (all-aligns (cl-mapcan #'argc-test--collect-display-aligns after-strings)))
      (should (cl-some #'argc-test--aligns-to-right-p all-aligns)))))

(ert-deftest argc-test-border-fills-with-dashes ()
  "Top/bottom border SHALL draw its horizontal rule with real `─' glyphs.
A blank `(space :align-to ...)' stretch renders no visible rule, so the
fill SHALL be box-drawing chars spanning the requested WIDTH."
  (let* ((width 40)
         (plain (argc--make-border ?┌ ?┐ width))
         (labelled (argc--make-border ?┌ ?┐ width "main")))
    ;; No blank stretch standing in for the rule.
    (should-not (cl-some #'argc-test--aligns-to-right-p
                         (argc-test--collect-display-aligns plain)))
    (should-not (cl-some #'argc-test--aligns-to-right-p
                         (argc-test--collect-display-aligns labelled)))
    ;; Continuous run of `─' fills the width (not just the 2-char lead).
    (should (>= (cl-count ?─ plain) (- width 3)))
    (should (>= (cl-count ?─ labelled) 3))
    ;; Border occupies exactly WIDTH display columns.
    (should (= (string-width plain) width))
    (should (= (string-width labelled) width))
    ;; Label still present, right-aligned before the corner.
    (should (string-match-p "main" labelled))))

(ert-deftest argc-test-top-border-spans-box-width ()
  "The live top border before-string SHALL span `argc--box-width' columns."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let* ((ov (cl-find-if (lambda (o) (and (overlay-get o 'argc-box)
                                            (overlay-get o 'before-string)))
                           (overlays-in (point-min) (point-max))))
           (bs (overlay-get ov 'before-string)))
      (should bs)
      ;; before-string is BORDER + "\n"; measure up to the newline.
      (let ((border (substring bs 0 (string-match "\n" bs))))
        (should (= (string-width border) (argc--box-width)))))))

(ert-deftest argc-test-mode-rebuilds-on-window-size-change ()
  "Enabling argc-mode SHALL schedule a rebuild on window configuration change
so width-sized borders re-fit after a resize."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc-mode 1)
    (should (memq #'argc--schedule-rebuild
                  (buffer-local-value 'window-configuration-change-hook
                                      (current-buffer))))))

(ert-deftest argc-test-per-line-wrap-prefix-has-rail ()
  "Per-line overlays SHALL carry a `wrap-prefix' containing the rail `│ '."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let ((ovs (cl-remove-if-not
                (lambda (ov) (overlay-get ov 'argc-box))
                (overlays-in (point-min) (point-max)))))
      (should (cl-some (lambda (ov)
                         (let ((wp (overlay-get ov 'wrap-prefix)))
                           (and (stringp wp) (string-match-p "│ " wp))))
                       ovs)))))

(ert-deftest argc-test-after-string-ends-with-default-face-mask ()
  "Per-line after-string body SHALL end with `(space :align-to right)' default."
  (with-temp-buffer
    (insert "# @cmd Foo\n# @arg bar!\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let* ((ovs (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'argc-box))
                 (overlays-in (point-min) (point-max))))
           (after-strings (delq nil (mapcar (lambda (ov) (overlay-get ov 'after-string)) ovs))))
      (should
       (cl-some
        (lambda (s)
          ;; Inspect the segment before any embedded newline (bottom border
          ;; tail).  The body-line mask SHALL be its final char.
          (let* ((nl (string-match "\n" s))
                 (body-end (1- (or nl (length s))))
                 (disp (get-text-property body-end 'display s))
                 (face (get-text-property body-end 'face s)))
            (and (consp disp)
                 (eq (car disp) 'space)
                 (argc-test--aligns-to-right-p (plist-get (cdr disp) :align-to))
                 (eq face 'default))))
        after-strings)))))

(ert-deftest argc-test-leading-hash-replaced-with-rail ()
  "Each body line's leading `#' SHALL be replaced by an evaporating overlay
whose `display' is `│' painted in the normalised box face."
  (with-temp-buffer
    (insert "# @cmd Foo\n# @arg bar!\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let ((hash-ovs
           (cl-remove-if-not
            (lambda (ov)
              (and (overlay-get ov 'argc-box)
                   (= (- (overlay-end ov) (overlay-start ov)) 1)
                   (eq (char-after (overlay-start ov)) ?#)
                   (overlay-get ov 'evaporate)
                   (let ((disp (overlay-get ov 'display)))
                     (and (stringp disp) (string-match-p "│" disp)))))
            (overlays-in (point-min) (point-max)))))
      (should (= 2 (length hash-ovs))))))

(ert-deftest argc-test-hash-reveal-on-point ()
  "Point on the substitution overlay SHALL hide its `display' so `#' shows;
moving off SHALL restore the `│' display."
  (with-temp-buffer
    (insert "# @cmd Foo\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let ((sub (cl-find-if (lambda (ov) (overlay-get ov 'argc-hash-sub))
                           (overlays-in (point-min) (point-max)))))
      (should sub)
      (goto-char (overlay-start sub))
      (argc--reveal-hash-at-point)
      (should (null (overlay-get sub 'display)))
      (goto-char (overlay-end sub))
      (argc--reveal-hash-at-point)
      (let ((disp (overlay-get sub 'display)))
        (should (and (stringp disp) (string-match-p "│" disp)))))))

(ert-deftest argc-test-no-before-string-rail-on-body-lines ()
  "Body lines SHALL NOT carry a `before-string' rail — substitution replaces it.
Only the first line may carry a `before-string' (for the top border)."
  (with-temp-buffer
    (insert "# @cmd Foo\n# @arg bar!\nfoo() {\n}\n")
    (argc--apply-box-overlays)
    (let* ((line-ovs
            (cl-remove-if-not
             (lambda (ov)
               (and (overlay-get ov 'argc-box)
                    (> (- (overlay-end ov) (overlay-start ov)) 1)))
             (overlays-in (point-min) (point-max))))
           (with-rail
            (cl-count-if
             (lambda (ov)
               (let ((bs (overlay-get ov 'before-string)))
                 (and bs (string-match-p "│ " bs))))
             line-ovs)))
      ;; Only one before-string carrying the rail is permitted — the first
      ;; line, whose before-string is the top border followed by a newline.
      ;; With the substitution approach even that is gone; allow zero or one.
      (should (<= with-rail 0)))))

(ert-deftest argc-test-normalised-box-face-clears-prose-styling ()
  "`argc--normalised-box-face' SHALL clear text-styling attrs and pin bg."
  (let ((spec (argc--normalised-box-face)))
    (should (eq (plist-get spec :slant) 'normal))
    (should (null (plist-get spec :underline)))
    (should (null (plist-get spec :overline)))
    (should (null (plist-get spec :strike-through)))
    (should (null (plist-get spec :box)))
    (should (equal (plist-get spec :background) "unspecified-bg"))
    (let ((inh (plist-get spec :inherit)))
      (should (or (eq inh 'argc-box-face)
                  (and (listp inh) (memq 'argc-box-face inh)))))))


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
