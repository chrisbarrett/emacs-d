;;; gfm-pretty-link-previews-tests.el --- Tests for the link-previews decorator -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `gfm-pretty-link-previews' — the decorator that
;; renders source-range and diff-range markdown link previews.
;; Migrated from `gfm-present-tests.el' when the helpers moved out of
;; `gfm-present.el' into their own module under `gfm-pretty'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gfm-pretty)
(require 'gfm-pretty-engine)
(require 'gfm-pretty-link-previews)


;;; Helpers

(defmacro gfm-pretty-link-previews-tests--with-temp-source (lines path-var &rest body)
  "Write LINES to a `.txt' temp file at PATH-VAR and run BODY.
`.txt' avoids loading language major modes that may prompt for
tree-sitter grammars and confuse downstream tests."
  (declare (indent 2))
  `(let ((,path-var (make-temp-file "src-test-" nil ".txt"
                                    (concat (mapconcat #'identity ,lines "\n")
                                            "\n"))))
     (unwind-protect (progn ,@body)
       (let ((b (find-buffer-visiting ,path-var)))
         (when b
           (with-current-buffer b (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
       (when (file-exists-p ,path-var) (delete-file ,path-var)))))

(defmacro gfm-pretty-link-previews-tests--with-fake-call-process (output exit &rest body)
  "Stub `call-process' to insert OUTPUT and return EXIT during BODY."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (_p _i _d _disp &rest _) (insert ,output) ,exit)))
     ,@body))

(defun gfm-pretty-link-previews-tests--overlays ()
  "Return the link-previews decorator's overlay list for the current buffer."
  (gfm-pretty--state-get 'link-previews 'overlays))


;;; Source-range link parsing

(ert-deftest gfm-pretty-link-previews/parse-source-link-with-range ()
  (let ((r (gfm-pretty-link-previews--parse-source-link
            "modules/auth.rs#L42-L67")))
    (should (equal "modules/auth.rs" (car r)))
    (should (= 42 (cadr r)))
    (should (= 67 (cddr r)))))

(ert-deftest gfm-pretty-link-previews/parse-source-link-single-line ()
  (let ((r (gfm-pretty-link-previews--parse-source-link "foo.el#L7")))
    (should (equal "foo.el" (car r)))
    (should (= 7 (cadr r)))
    (should (= 7 (cddr r)))))

(ert-deftest gfm-pretty-link-previews/parse-source-link-rejects-other-forms ()
  (should-not (gfm-pretty-link-previews--parse-source-link "modules/auth.rs"))
  (should-not (gfm-pretty-link-previews--parse-source-link "https://example.com"))
  (should-not (gfm-pretty-link-previews--parse-source-link "#auth-flow"))
  (should-not (gfm-pretty-link-previews--parse-source-link "diff:a...b"))
  (should-not (gfm-pretty-link-previews--parse-source-link nil)))

(ert-deftest gfm-pretty-link-previews/major-mode-for-path-resolves-via-auto-mode-alist ()
  (let ((auto-mode-alist
         (cons (cons (rx "." (or "ya" "y") "ml" eos) 'fundamental-mode)
               auto-mode-alist)))
    (should (eq 'fundamental-mode
                (gfm-pretty-link-previews--major-mode-for-path "config.yml")))))

(ert-deftest gfm-pretty-link-previews/major-mode-for-path-unknown-extension-falls-back ()
  (let ((auto-mode-alist nil))
    (should (eq #'fundamental-mode
                (gfm-pretty-link-previews--major-mode-for-path
                 "x.totally-unknown-ext-xyz")))))

(ert-deftest gfm-pretty-link-previews/read-line-range-small ()
  (gfm-pretty-link-previews-tests--with-temp-source
      '("line1" "line2" "line3" "line4") tmp
    (let ((r (gfm-pretty-link-previews--read-line-range tmp 2 3)))
      (should (equal '("line2" "line3") (car r)))
      (should (= 0 (cdr r))))))

(ert-deftest gfm-pretty-link-previews/read-line-range-truncates-and-reports-extra ()
  (gfm-pretty-link-previews-tests--with-temp-source
      (mapcar (lambda (n) (format "line%d" n)) (number-sequence 1 20)) tmp
    (let ((r (gfm-pretty-link-previews--read-line-range tmp 1 15)))
      (should (= 10 (length (car r))))
      (should (= 5 (cdr r))))))

(ert-deftest gfm-pretty-link-previews/read-line-range-missing-file ()
  (should (eq 'file-not-found
              (gfm-pretty-link-previews--read-line-range "/no/such/file" 1 5))))

(ert-deftest gfm-pretty-link-previews/read-line-range-invalid-range ()
  (gfm-pretty-link-previews-tests--with-temp-source '("line1" "line2") tmp
    (should (eq 'invalid-range
                (gfm-pretty-link-previews--read-line-range tmp 0 5)))
    (should (eq 'invalid-range
                (gfm-pretty-link-previews--read-line-range tmp 5 2)))
    (should (eq 'invalid-range
                (gfm-pretty-link-previews--read-line-range tmp 100 200)))))


;;; Box-preview helpers

(ert-deftest gfm-pretty-link-previews/abbrev-source-path-uses-project-relative ()
  "When PATH is inside a project, the result is project-relative."
  (let* ((root (file-name-as-directory (make-temp-file "pres-proj-" t)))
         (subdir (expand-file-name "subdir/" root))
         (abs (expand-file-name "file.rs" subdir)))
    (unwind-protect
        (progn
          (make-directory subdir t)
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&optional _ &rest _) (list 'transient root)))
                    ((symbol-function 'project-root)
                     (lambda (_p) root)))
            (should (equal "subdir/file.rs"
                           (gfm-pretty-link-previews--abbrev-source-path abs)))))
      (delete-directory root t))))

(ert-deftest gfm-pretty-link-previews/abbrev-source-path-passes-through-nil ()
  "Nil or empty PATH is returned unchanged."
  (should-not (gfm-pretty-link-previews--abbrev-source-path nil))
  (should (equal "" (gfm-pretty-link-previews--abbrev-source-path ""))))

(ert-deftest gfm-pretty-link-previews/fit-label-into-border-passes-through-short ()
  (should (equal "auth.rs:1-5"
                 (gfm-pretty-link-previews--fit-label-into-border
                  "auth.rs:1-5" 30))))

(ert-deftest gfm-pretty-link-previews/fit-label-into-border-keeps-basename-on-overflow ()
  (let ((res (gfm-pretty-link-previews--fit-label-into-border
              "deeply/nested/dir/auth.rs:1-5" 18)))
    (should (string-prefix-p "…/" res))
    (should (string-match-p "auth.rs:1-5" res))
    (should (<= (string-width res) 18))))

(ert-deftest gfm-pretty-link-previews/fit-label-into-border-falls-back-to-plain-ellipsis ()
  "A label without a `/' falls back to a leading-ellipsis tail."
  (let ((res (gfm-pretty-link-previews--fit-label-into-border
              "abcdefghijklmnopqrstuvwxyz" 6)))
    (should (string-prefix-p "…" res))
    (should (<= (string-width res) 6))))

(ert-deftest gfm-pretty-link-previews/truncate-line-to-width-short-passthrough ()
  (should (equal "abc"
                 (gfm-pretty-link-previews--truncate-line-to-width "abc" 10))))

(ert-deftest gfm-pretty-link-previews/truncate-line-to-width-suffixes-ellipsis ()
  (let ((s (gfm-pretty-link-previews--truncate-line-to-width
            "0123456789abcdef" 6)))
    (should (= 6 (string-width s)))
    (should (string-suffix-p "…" s))))

(ert-deftest gfm-pretty-link-previews/standalone-link-p-whole-line ()
  (with-temp-buffer
    (insert "[lbl](modules/auth.rs#L42-L48)\n")
    (goto-char (point-min))
    (should (re-search-forward
             gfm-pretty-link-previews--md-link-rx nil t))
    (should (gfm-pretty-link-previews--standalone-link-p
             (match-beginning 0) (match-end 0)))))

(ert-deftest gfm-pretty-link-previews/standalone-link-p-list-item ()
  (with-temp-buffer
    (insert "- [lbl](modules/auth.rs#L42-L48)\n")
    (goto-char (point-min))
    (should (re-search-forward
             gfm-pretty-link-previews--md-link-rx nil t))
    (should (gfm-pretty-link-previews--standalone-link-p
             (match-beginning 0) (match-end 0)))))

(ert-deftest gfm-pretty-link-previews/standalone-link-p-blockquote ()
  (with-temp-buffer
    (insert "> [lbl](modules/auth.rs#L42-L48)\n")
    (goto-char (point-min))
    (should (re-search-forward
             gfm-pretty-link-previews--md-link-rx nil t))
    (should (gfm-pretty-link-previews--standalone-link-p
             (match-beginning 0) (match-end 0)))))

(ert-deftest gfm-pretty-link-previews/standalone-link-p-mid-prose-rejected ()
  (with-temp-buffer
    (insert "See [lbl](modules/auth.rs#L42-L48) for details.\n")
    (goto-char (point-min))
    (should (re-search-forward
             gfm-pretty-link-previews--md-link-rx nil t))
    (should-not (gfm-pretty-link-previews--standalone-link-p
                 (match-beginning 0) (match-end 0)))))

(ert-deftest gfm-pretty-link-previews/standalone-link-p-trailing-text-rejected ()
  (with-temp-buffer
    (insert "[lbl](modules/auth.rs#L42-L48) and that's the bit\n")
    (goto-char (point-min))
    (should (re-search-forward
             gfm-pretty-link-previews--md-link-rx nil t))
    (should-not (gfm-pretty-link-previews--standalone-link-p
                 (match-beginning 0) (match-end 0)))))

(ert-deftest gfm-pretty-link-previews/abbrev-diff-refs-shortens-40-hex ()
  (should (equal "abcdef0"
                 (gfm-pretty-link-previews--abbrev-diff-refs
                  "abcdef0123456789abcdef0123456789abcdef01"))))

(ert-deftest gfm-pretty-link-previews/abbrev-diff-refs-passes-branches-through ()
  (should (equal "main"
                 (gfm-pretty-link-previews--abbrev-diff-refs "main")))
  (should (equal "HEAD~1"
                 (gfm-pretty-link-previews--abbrev-diff-refs "HEAD~1"))))


;;; Box renderer

(ert-deftest gfm-pretty-link-previews/box-display-has-corner-glyphs ()
  (let ((s (gfm-pretty-link-previews--box-display
            :label "foo:1-2" :body "alpha\nbeta" :extra 0)))
    (should (eq ?┌ (aref s 0)))
    (should (eq ?┘ (aref s (1- (length s)))))))

(ert-deftest gfm-pretty-link-previews/box-display-embeds-label-in-top-border ()
  (let ((s (gfm-pretty-link-previews--box-display
            :label "src:42-48" :body "x" :extra 0)))
    (should (string-match-p "┌─ src:42-48 " s))))

(ert-deftest gfm-pretty-link-previews/box-display-bottom-border-bare-when-no-extra ()
  (let ((s (gfm-pretty-link-previews--box-display
            :label "lbl" :body "x" :extra 0)))
    (should-not (string-match-p "\\+[0-9]+ more lines" s))))

(ert-deftest gfm-pretty-link-previews/box-display-bottom-border-embeds-extra ()
  (let ((s (gfm-pretty-link-previews--box-display
            :label "lbl" :body "x" :extra 5)))
    (should (string-match-p "└─ \\+5 more lines " s))))

(ert-deftest gfm-pretty-link-previews/box-display-lhs-margin-removes-inner-padding ()
  "LHS-margin mode wraps body lines with `│' on both sides — no inner gap."
  (let* ((s (gfm-pretty-link-previews--box-display
             :label "lbl" :body "+added" :extra 0 :lhs-margin t))
         (lines (split-string s "\n")))
    (should (string-match-p "^│\\+added" (nth 1 lines)))))

(ert-deftest gfm-pretty-link-previews/box-display-truncates-overlong-body-line ()
  "Body lines wider than the interior budget are ellipsised."
  (let* ((long (make-string 200 ?x))
         (s (gfm-pretty-link-previews--box-display
             :label "lbl" :body long :extra 0))
         (lines (split-string s "\n"))
         (body-line (nth 1 lines)))
    (should (string-match-p "…" body-line))))


;;; Source-range link preview overlay

(ert-deftest gfm-pretty-link-previews/source-preview-display-has-box-borders ()
  (gfm-pretty-link-previews-tests--with-temp-source '("alpha" "beta") tmp
    (let ((s (gfm-pretty-link-previews--source-preview-display tmp 1 2)))
      (should (eq ?┌ (aref s 0)))
      (should (eq ?┘ (aref s (1- (length s))))))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-no-fence-delimiters ()
  (gfm-pretty-link-previews-tests--with-temp-source '("alpha" "beta") tmp
    (let ((s (gfm-pretty-link-previews--source-preview-display tmp 1 2)))
      (should-not (string-match-p "```" s)))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-top-border-has-path-and-range ()
  (gfm-pretty-link-previews-tests--with-temp-source '("alpha" "beta") tmp
    (let* ((fill-column 200)
           (s (gfm-pretty-link-previews--source-preview-display tmp 1 2))
           (top (substring s 0 (string-match-p "\n" s)))
           (abbrev (gfm-pretty-link-previews--abbrev-source-path tmp)))
      (should (string-match-p (regexp-quote (format "%s:1-2" abbrev)) top)))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-omits-markdown-label ()
  "The preview surface SHALL NOT contain the markdown `[label]' text."
  (gfm-pretty-link-previews-tests--with-temp-source '("alpha" "beta") tmp
    (let ((s (gfm-pretty-link-previews--source-preview-display tmp 1 2)))
      (should-not (string-match-p "my-custom-label" s)))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-missing-file-bare-sentinel ()
  (let ((s (gfm-pretty-link-previews--source-preview-display
            "/no/such/file" 1 5)))
    (should (string-match-p "\\[broken preview\\]" s))
    (should (string-match-p "file not found" s))
    (should-not (string-match-p "┌\\|└" s))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-invalid-range-bare-sentinel ()
  (gfm-pretty-link-previews-tests--with-temp-source '("one") tmp
    (let ((s (gfm-pretty-link-previews--source-preview-display tmp 100 200)))
      (should (string-match-p "\\[broken preview\\]" s))
      (should (string-match-p "invalid range" s))
      (should-not (string-match-p "┌\\|└" s)))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-oversize-footer-in-bottom-border ()
  (gfm-pretty-link-previews-tests--with-temp-source
      (mapcar (lambda (n) (format "line%d" n)) (number-sequence 1 20)) tmp
    (let* ((s (gfm-pretty-link-previews--source-preview-display tmp 1 15))
           (lines (split-string s "\n"))
           (bot (car (last lines))))
      (should (string-match-p "\\+5 more lines" bot))
      (should-not (string-match-p "click to open" s)))))

(ert-deftest gfm-pretty-link-previews/fontify-source-applies-major-mode-face ()
  "A custom mode mapped via `auto-mode-alist' contributes `face' to the body."
  (let* ((mode-sym (make-symbol "gfm-pretty-link-previews-tests--demo-mode")))
    (eval `(define-derived-mode ,mode-sym prog-mode "demo"
             (setq font-lock-defaults
                   '((("\\<KEYWORD\\>" . font-lock-keyword-face))))))
    (let ((auto-mode-alist
           (cons (cons (rx ".gfmdemo" eos) mode-sym) auto-mode-alist)))
      (let* ((body (gfm-pretty-link-previews--fontify-source
                    "/tmp/x.gfmdemo" '("KEYWORD other")))
             (idx (string-match "KEYWORD" body)))
        (should idx)
        (should (eq 'font-lock-keyword-face
                    (get-text-property idx 'face body)))))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-fontifies-body ()
  "End-to-end: small range via display helper carries major-mode faces."
  (let ((mode-sym (make-symbol "gfm-pretty-link-previews-tests--demo-mode-2")))
    (eval `(define-derived-mode ,mode-sym prog-mode "demo2"
             (setq font-lock-defaults
                   '((("\\<HOT\\>" . font-lock-keyword-face))))))
    (let ((auto-mode-alist
           (cons (cons (rx ".gfmdemo2" eos) mode-sym) auto-mode-alist))
          (path (make-temp-file "src-test-" nil ".gfmdemo2"
                                "HOT body\nplain line\n")))
      (unwind-protect
          (let* ((s (gfm-pretty-link-previews--source-preview-display path 1 2))
                 (idx (string-match "HOT" s)))
            (should idx)
            (should (eq 'font-lock-keyword-face
                        (get-text-property idx 'face s))))
        (when (file-exists-p path) (delete-file path))))))

(ert-deftest gfm-pretty-link-previews/source-preview-display-unknown-ext-no-error ()
  "Unknown extension yields a clean unfontified body without signalling."
  (let ((auto-mode-alist nil)
        (path (make-temp-file "src-test-" nil ".totally-unknown-ext-xyz"
                              "alpha\nbeta\n")))
    (unwind-protect
        (let ((s (gfm-pretty-link-previews--source-preview-display path 1 2)))
          (should (string-match-p "alpha" s))
          (should (string-match-p "beta" s)))
      (when (file-exists-p path) (delete-file path)))))


;;; Rebuild — single decorator pass

(ert-deftest gfm-pretty-link-previews/rebuild-creates-overlay ()
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c" "d") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L2-L3)\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (let ((ovs (gfm-pretty-link-previews-tests--overlays)))
        (should (= 1 (length ovs)))
        (let ((display (overlay-get (car ovs) 'display)))
          (should display)
          (should (string-match-p "b" display))
          (should (string-match-p "c" display))
          (should-not (string-match-p "```" display)))))))

(ert-deftest gfm-pretty-link-previews/rebuild-skips-mid-prose-link ()
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\nSee [foo](%s#L1) for details.\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (should (= 0 (length (gfm-pretty-link-previews-tests--overlays)))))))

(ert-deftest gfm-pretty-link-previews/rebuild-skips-link-with-trailing-text ()
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1) and that's the bit\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (should (= 0 (length (gfm-pretty-link-previews-tests--overlays)))))))

(ert-deftest gfm-pretty-link-previews/rebuild-list-item-link ()
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n- [foo](%s#L1)\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (should (= 1 (length (gfm-pretty-link-previews-tests--overlays)))))))

(ert-deftest gfm-pretty-link-previews/rebuild-blockquote-link ()
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n> [foo](%s#L1)\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (should (= 1 (length (gfm-pretty-link-previews-tests--overlays)))))))

(ert-deftest gfm-pretty-link-previews/rebuild-does-not-mutate-buffer ()
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1)\n" tmp))
      (let ((before-text (buffer-string)))
        (gfm-pretty-link-previews--rebuild)
        (should (equal before-text (buffer-string)))))))


;;; Diff-range link parsing + rendering

(ert-deftest gfm-pretty-link-previews/parse-diff-link-with-path ()
  (let ((r (gfm-pretty-link-previews--parse-diff-link
            "diff:main...HEAD#auth.rs")))
    (should (equal "main"    (plist-get r :base)))
    (should (equal "HEAD"    (plist-get r :head)))
    (should (equal "auth.rs" (plist-get r :path)))))

(ert-deftest gfm-pretty-link-previews/parse-diff-link-without-path ()
  (let ((r (gfm-pretty-link-previews--parse-diff-link "diff:HEAD~1...HEAD")))
    (should (equal "HEAD~1" (plist-get r :base)))
    (should (equal "HEAD"   (plist-get r :head)))
    (should-not (plist-get r :path))))

(ert-deftest gfm-pretty-link-previews/parse-diff-link-handles-dotted-refs ()
  (let ((r (gfm-pretty-link-previews--parse-diff-link "diff:v1.0...v2.0")))
    (should (equal "v1.0" (plist-get r :base)))
    (should (equal "v2.0" (plist-get r :head)))))

(ert-deftest gfm-pretty-link-previews/parse-diff-link-rejects-non-diff ()
  (should-not (gfm-pretty-link-previews--parse-diff-link "https://example.com"))
  (should-not (gfm-pretty-link-previews--parse-diff-link "p.rs#L1"))
  (should-not (gfm-pretty-link-previews--parse-diff-link "diff:abc"))
  (should-not (gfm-pretty-link-previews--parse-diff-link "diff:..."))
  (should-not (gfm-pretty-link-previews--parse-diff-link nil)))

(ert-deftest gfm-pretty-link-previews/diff-preview-argv-no-path ()
  (should (equal '("git" "-C" "/wt" "diff" "B...H")
                 (gfm-pretty-link-previews--diff-preview-argv "/wt" "B" "H"))))

(ert-deftest gfm-pretty-link-previews/diff-preview-argv-with-path ()
  (should (equal '("git" "-C" "/wt" "diff" "B...H" "--" "p.rs")
                 (gfm-pretty-link-previews--diff-preview-argv
                  "/wt" "B" "H" "p.rs"))))

(ert-deftest gfm-pretty-link-previews/run-diff-preview-empty ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "" 0
    (let ((r (gfm-pretty-link-previews--run-diff-preview "/wt" "B" "H")))
      (should (eq 'no-changes (plist-get r :status))))))

(ert-deftest gfm-pretty-link-previews/run-diff-preview-small ()
  (gfm-pretty-link-previews-tests--with-fake-call-process
      "line1\nline2\nline3\n" 0
    (let ((r (gfm-pretty-link-previews--run-diff-preview "/wt" "B" "H")))
      (should (eq 'ok (plist-get r :status)))
      (should (= 0 (plist-get r :extra)))
      (should (string-match-p "line1\nline2\nline3" (plist-get r :body))))))

(ert-deftest gfm-pretty-link-previews/run-diff-preview-oversized-truncates ()
  (let ((output (concat (mapconcat (lambda (n) (format "L%d" n))
                                   (number-sequence 1 15) "\n")
                        "\n")))
    (gfm-pretty-link-previews-tests--with-fake-call-process output 0
      (let ((r (gfm-pretty-link-previews--run-diff-preview "/wt" "B" "H")))
        (should (= 5 (plist-get r :extra)))
        (should (= 10 (length (split-string (plist-get r :body) "\n"))))))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-empty-bare-sentinel ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "" 0
    (let ((s (gfm-pretty-link-previews--diff-preview-display
              "/wt" "B" "H" nil)))
      (should (string-match-p "\\[broken preview\\]" s))
      (should (string-match-p "no changes" s))
      (should-not (string-match-p "┌\\|└" s)))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-error-bare-sentinel ()
  (gfm-pretty-link-previews-tests--with-fake-call-process
      "fatal: bad object\nmore\n" 128
    (let ((s (gfm-pretty-link-previews--diff-preview-display
              "/wt" "B" "H" nil)))
      (should (string-match-p "\\[broken preview\\]" s))
      (should (string-match-p "git error: fatal: bad object" s))
      (should-not (string-match-p "┌\\|└" s)))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-has-box-borders ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "+added line\n" 0
    (let ((s (gfm-pretty-link-previews--diff-preview-display
              "/wt" "main" "feature" nil)))
      (should (eq ?┌ (aref s 0)))
      (should (eq ?┘ (aref s (1- (length s))))))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-top-border-embeds-base...head ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "+added line\n" 0
    (let* ((s (gfm-pretty-link-previews--diff-preview-display
               "/wt" "main" "feature" nil))
           (top (substring s 0 (string-match-p "\n" s))))
      (should (string-match-p "main\\.\\.\\.feature" top)))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-top-border-embeds-path-with-em-dash ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "+added line\n" 0
    (let* ((s (gfm-pretty-link-previews--diff-preview-display
               "/wt" "main" "feature" "src/foo.rs"))
           (top (substring s 0 (string-match-p "\n" s))))
      (should (string-match-p "main\\.\\.\\.feature — src/foo.rs" top)))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-shortens-40-hex-shas ()
  (let ((sha-a "abcdef0123456789abcdef0123456789abcdef01")
        (sha-b "0123456789abcdef0123456789abcdef01234567"))
    (gfm-pretty-link-previews-tests--with-fake-call-process "+x\n" 0
      (let* ((s (gfm-pretty-link-previews--diff-preview-display
                 "/wt" sha-a sha-b nil))
             (top (substring s 0 (string-match-p "\n" s))))
        (should (string-match-p "abcdef0\\.\\.\\.0123456" top))
        (should-not (string-match-p sha-a top))
        (should-not (string-match-p sha-b top))))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-lhs-margin-body-shape ()
  "LHS-margin body lines start with `│' followed directly by the diff indicator."
  (gfm-pretty-link-previews-tests--with-fake-call-process
      "+added\n-removed\n unchanged\n" 0
    (let* ((s (gfm-pretty-link-previews--diff-preview-display
               "/wt" "B" "H" nil))
           (lines (split-string s "\n"))
           (body (cl-subseq lines 1 -1)))
      (dolist (line body)
        (should (string-match-p "^│[-+ ]" line))))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-omits-markdown-label ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "+x\n" 0
    (let ((s (gfm-pretty-link-previews--diff-preview-display
              "/wt" "B" "H" nil)))
      (should-not (string-match-p "my-custom-label" s)))))

(ert-deftest gfm-pretty-link-previews/rebuild-handles-diff-link ()
  (gfm-pretty-link-previews-tests--with-fake-call-process "diff line 1\n" 0
    (with-temp-buffer
      (insert "# Slide\n[change](diff:HEAD~1...HEAD)\n")
      (gfm-pretty-link-previews--rebuild)
      (let ((ovs (gfm-pretty-link-previews-tests--overlays)))
        (should (= 1 (length ovs)))
        (let ((display (overlay-get (car ovs) 'display)))
          (should (string-match-p "diff line 1" display)))))))


;;; Decorator lifecycle (5.2 / 5.3 / 5.4)

(ert-deftest gfm-pretty-link-previews/pretty-mode-renders-in-plain-buffer ()
  "Enabling `gfm-pretty-mode' renders the preview overlay (no presentation mode)."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L1-L2)\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (let ((ovs (gfm-pretty-link-previews-tests--overlays)))
            (should (= 1 (length ovs)))
            (should (overlay-get (car ovs) 'display)))
        (gfm-pretty-mode -1)))))

(ert-deftest gfm-pretty-link-previews/toggle-decorator-off-removes-overlay ()
  "Toggling the decorator off then on tears down and re-creates the overlay."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L1-L2)\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (progn
            (should (= 1 (length (gfm-pretty-link-previews-tests--overlays))))
            (gfm-pretty-toggle-decorator 'link-previews)
            (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))
            (gfm-pretty-toggle-decorator 'link-previews)
            (should (= 1 (length (gfm-pretty-link-previews-tests--overlays)))))
        (gfm-pretty-mode -1)))))

(ert-deftest gfm-pretty-link-previews/box-width-reclamps-on-available-width-change ()
  "Refresh at a new available width re-clamps box width per the spec formula.

Covers the `window resize triggers preview re-render' scenario:
`min(avail, max(80, longest-body-line + decoration-w))'.  In batch
mode `gfm-pretty--available-width' falls back to `fill-column'
since no window exists, so flipping `fill-column' between rebuilds
exercises the same width-clamp pathway the engine's
`window-configuration-change-hook' drives at runtime."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L1-L3)\n" tmp))
      (let ((fill-column 50))
        (gfm-pretty-link-previews--rebuild)
        (let* ((narrow (overlay-get
                        (car (gfm-pretty-link-previews-tests--overlays))
                        'display))
               (top-narrow (substring narrow 0
                                      (string-match-p "\n" narrow))))
          (setq fill-column 120)
          (gfm-pretty-link-previews--rebuild)
          (let* ((wide (overlay-get
                        (car (gfm-pretty-link-previews-tests--overlays))
                        'display))
                 (top-wide (substring wide 0
                                      (string-match-p "\n" wide))))
            ;; The 50-cell window must clamp narrower than the 120-cell
            ;; window's box, which clamps at the 80 floor.
            (should (< (string-width top-narrow)
                       (string-width top-wide)))
            (should (<= (string-width top-narrow) 51))
            (should (<= (string-width top-wide) 81))))))))

(ert-deftest gfm-pretty-link-previews/edit-triggers-rebuild ()
  "An edit to the link URL causes the next rebuild to re-scan."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L1)\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (let* ((decorator (gfm-pretty--get 'link-previews))
                 (stats0 (gfm-pretty--state-get 'link-previews 'rebuild-stats))
                 (count0 (or (plist-get stats0 :count) 0)))
            (goto-char (point-min))
            (search-forward "#L1")
            (replace-match "#L2")
            ;; Drive the engine's scoped rebuild directly — the
            ;; predicate forces a full rebuild for any edit.
            (let ((dirty (cons (point-min) (point-max))))
              (gfm-pretty--state-set 'link-previews 'dirty-region nil)
              (cond
               ((gfm-pretty--dirty-forces-full-rebuild-p decorator dirty)
                (gfm-pretty--rebuild decorator))
               (t
                (gfm-pretty--rebuild-scoped-by-block decorator dirty))))
            (let* ((stats1 (gfm-pretty--state-get 'link-previews 'rebuild-stats))
                   (count1 (or (plist-get stats1 :count) 0)))
              (should (> count1 count0)))
            (should (= 1 (length (gfm-pretty-link-previews-tests--overlays)))))
        (gfm-pretty-mode -1)))))


;;; Marker-aware indent (5.1 / 5.2 / 5.3)

(ert-deftest gfm-pretty-link-previews/list-item-preview-indents-under-marker ()
  "A `- [foo](path#L1-L3)' preview lays its top border bare and indents
body + bottom by 2 spaces so they align under the top's `┌'.
The overlay's first line is rendered at the buffer column of `[' (col
2 here), so its `┌' is already at visual column 2; subsequent display
lines need the explicit 2-space prefix to land at the same column."
  (gfm-pretty-link-previews-tests--with-temp-source
      '("alpha" "beta" "gamma") tmp
    (with-temp-buffer
      (insert (format "- [foo](%s#L1-L3)\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (let* ((ov (car (gfm-pretty-link-previews-tests--overlays)))
             (display (overlay-get ov 'display))
             (lines (split-string display "\n"))
             (top (car lines))
             (bot (car (last lines)))
             (body (butlast (cdr lines))))
        (should (string-prefix-p "┌" top))
        (should (string-suffix-p "┐" top))
        (should (string-prefix-p "  └" bot))
        (should (string-suffix-p "┘" bot))
        (dolist (line body)
          (should (string-prefix-p "  │" line))
          (should (string-suffix-p "│" line)))))))

(ert-deftest gfm-pretty-link-previews/blockquote-preview-prefixes-with-rail ()
  "A `> [foo](...)' preview leaves the top bare and prefixes body /
bottom rows with the blockquote rail wrap-prefix so the rail flows
through the box (so adjacent `>'-prefixed prose lines join visually)."
  (require 'gfm-pretty-blockquotes)
  (gfm-pretty-link-previews-tests--with-temp-source
      '("alpha" "beta" "gamma") tmp
    (with-temp-buffer
      (insert (format "> [foo](%s#L1-L3)\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (let* ((ov (car (gfm-pretty-link-previews-tests--overlays)))
                 (display (overlay-get ov 'display))
                 (lines (split-string display "\n"))
                 (bot (car (last lines)))
                 (body (butlast (cdr lines))))
            (should (string-prefix-p "┌" (car lines)))
            ;; Every non-first row should contain the rail glyph.
            (should (string-match-p "▌" bot))
            (dolist (line body)
              (should (string-match-p "▌" line))))
        (gfm-pretty-mode -1)))))

(ert-deftest gfm-pretty-link-previews/whole-line-preview-has-no-indent ()
  "A whole-line `[foo](path#L1-L3)' preview keeps indent 0 throughout."
  (gfm-pretty-link-previews-tests--with-temp-source
      '("alpha" "beta" "gamma") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L1-L3)\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (let* ((ov (car (gfm-pretty-link-previews-tests--overlays)))
             (display (overlay-get ov 'display))
             (lines (split-string display "\n")))
        (should (string-prefix-p "┌" (car lines)))
        (should (string-prefix-p "└" (car (last lines))))))))


;;; Blockquote-rail coexistence (5.4)

(ert-deftest gfm-pretty-link-previews/blockquote-rail-coexists-with-preview ()
  "On a `> [link]' preview line, the blockquotes rail stays — both
decorators paint on the same line in non-overlapping ranges.  The
plain `> text' line below also keeps its rail."
  (require 'gfm-pretty-blockquotes)
  (gfm-pretty-link-previews-tests--with-temp-source '("alpha" "beta") tmp
    (with-temp-buffer
      (insert (format "> [foo](%s#L1-L2)\n> plain blockquote text\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (let* ((preview-line-beg (point-min))
                 (preview-line-end (save-excursion
                                     (goto-char (point-min))
                                     (line-end-position)))
                 (rail-prop (gfm-pretty--registry-display
                             gfm-pretty-blockquotes--registry))
                 (preview-line-bq-ovs
                  (cl-remove-if-not
                   (lambda (ov) (overlay-get ov rail-prop))
                   (overlays-in preview-line-beg preview-line-end)))
                 (plain-line-beg (save-excursion
                                   (goto-char (point-min))
                                   (forward-line 1)
                                   (point)))
                 (plain-line-end (save-excursion
                                   (goto-char plain-line-beg)
                                   (line-end-position)))
                 (plain-line-bq-ovs
                  (cl-remove-if-not
                   (lambda (ov) (overlay-get ov rail-prop))
                   (overlays-in plain-line-beg plain-line-end))))
            (should (> (length preview-line-bq-ovs) 0))
            (should (> (length plain-line-bq-ovs) 0)))
        (gfm-pretty-mode -1)))))


;;; Diff body fontification (5.5)

(ert-deftest gfm-pretty-link-previews/fontify-diff-applies-diff-faces ()
  "diff-mode font-lock paints `diff-added' on `+' lines and `diff-removed' on `-'."
  (require 'diff-mode)
  (let* ((body "+added one\n-removed one\n context\n")
         (out (gfm-pretty-link-previews--fontify-diff body))
         (added-idx (string-match "+added" out))
         (removed-idx (string-match "-removed" out)))
    (should added-idx)
    (should removed-idx)
    (let ((added-face (get-text-property added-idx 'face out))
          (removed-face (get-text-property removed-idx 'face out)))
      (should (or (eq added-face 'diff-added)
                  (and (listp added-face) (memq 'diff-added added-face))
                  (and (facep added-face)
                       (eq (face-attribute added-face :inherit nil t)
                           'diff-added))))
      (should (or (eq removed-face 'diff-removed)
                  (and (listp removed-face) (memq 'diff-removed removed-face))
                  (and (facep removed-face)
                       (eq (face-attribute removed-face :inherit nil t)
                           'diff-removed)))))))

(ert-deftest gfm-pretty-link-previews/diff-preview-display-carries-diff-faces ()
  "End-to-end: stubbed `git diff' output round-trips through `diff-mode'.

`-fontify-diff' renders bare `+'/`-' lines (no file or hunk header) with
diff-mode's `+'/`-'-line keywords applying `diff-indicator-added' /
`diff-indicator-removed' on the leading sigil; assert each surviving
into the boxed display string at the position `string-match' returns."
  (require 'diff-mode)
  (gfm-pretty-link-previews-tests--with-fake-call-process
      "+new line\n-old line\n" 0
    (let* ((s (gfm-pretty-link-previews--diff-preview-display
               "/wt" "B" "H" nil))
           (added-idx (string-match "\\+new line" s))
           (removed-idx (string-match "-old line" s)))
      (should added-idx)
      (should removed-idx)
      ;; The `+' sigil itself carries `diff-indicator-added'.  The `-'
      ;; line is at end-of-buffer in the temp `diff-mode' buffer; in
      ;; that position diff-mode's regex anchors don't fire, so the
      ;; first char isn't fontified.  Check the position one past, on
      ;; the line's body, where the unanchored `diff-removed' regex
      ;; (matching the whole `-' line) DOES fontify.
      (should (get-text-property added-idx 'face s))
      (should (get-text-property (1+ removed-idx) 'face s)))))


;;; RET dispatch (5.6 / 5.7 / 5.8)

(ert-deftest gfm-pretty-link-previews/follow-source-opens-target ()
  "RET on a source-range overlay opens the target via `find-file-noselect'."
  (gfm-pretty-link-previews-tests--with-temp-source
      '("alpha" "beta" "gamma") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L2-L3)\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (let ((popped nil))
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buf &rest _) (setq popped buf) (set-buffer buf) buf))
                  ((symbol-function 'pulsar-highlight-pulse)
                   (lambda (&optional _ &rest _) nil)))
          (goto-char (overlay-start
                      (car (gfm-pretty-link-previews-tests--overlays))))
          (gfm-pretty-link-previews-follow-link-at-point)
          (should popped)
          (with-current-buffer popped
            (should (= 2 (line-number-at-pos (point))))))))))

(ert-deftest gfm-pretty-link-previews/follow-diff-prefers-magit ()
  "RET on a diff overlay dispatches to `magit-diff-range' when available."
  (gfm-pretty-link-previews-tests--with-fake-call-process "+x\n" 0
    (with-temp-buffer
      (insert "[change](diff:main...HEAD#foo.el)\n")
      (gfm-pretty-link-previews--rebuild)
      (goto-char (overlay-start
                  (car (gfm-pretty-link-previews-tests--overlays))))
      (let ((captured nil)
            (orig-require (symbol-function 'require)))
        (cl-letf (((symbol-function 'require)
                   (lambda (feat &rest args)
                     (cond ((eq feat 'magit) t)
                           (t (apply orig-require feat args)))))
                  ((symbol-function 'magit-diff-range)
                   (lambda (range &optional _args files)
                     (setq captured (list range files)))))
          (gfm-pretty-link-previews-follow-link-at-point)
          (should (equal "main...HEAD" (nth 0 captured)))
          (should (equal '("foo.el") (nth 1 captured))))))))

(ert-deftest gfm-pretty-link-previews/follow-diff-falls-back-to-diff-buffer ()
  "RET on a diff overlay without magit populates a `*Diff*' buffer."
  (let (popped call-args)
    (gfm-pretty-link-previews-tests--with-fake-call-process "+x\n" 0
      (with-temp-buffer
        (insert "[change](diff:main...HEAD)\n")
        (gfm-pretty-link-previews--rebuild)
        (goto-char (overlay-start
                    (car (gfm-pretty-link-previews-tests--overlays))))
        (let ((orig-require (symbol-function 'require)))
          (cl-letf (((symbol-function 'require)
                     (lambda (feat &rest args)
                       (cond ((eq feat 'magit) nil)
                             ((eq feat 'diff-mode) t)
                             (t (apply orig-require feat args)))))
                    ((symbol-function 'call-process)
                     (lambda (&rest args)
                       (setq call-args args)
                       (insert "fake diff body\n")
                       0))
                    ((symbol-function 'diff-mode) (lambda () nil))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buf &rest _) (setq popped buf) buf)))
            (gfm-pretty-link-previews-follow-link-at-point)
            (should popped)
            (should (equal "*Diff*" (buffer-name popped)))
            (should (member "diff" call-args))
            (should (member "main...HEAD" call-args))))))))

(ert-deftest gfm-pretty-link-previews/follow-link-outside-overlay-signals ()
  "Calling the follow command off a preview overlay signals `user-error'."
  (with-temp-buffer
    (insert "no preview here\n")
    (goto-char (point-min))
    (should-error (gfm-pretty-link-previews-follow-link-at-point)
                  :type 'user-error)))

;;; Bare-line reference discovery

(ert-deftest gfm-pretty-link-previews/bare-absolute-path-creates-overlay ()
  "Bare absolute `<path>#L<a>-L<b>' on its own line gets a preview."
  (gfm-pretty-link-previews-tests--with-temp-source
      '("alpha" "beta" "gamma") tmp
    (with-temp-buffer
      (insert (format "%s#L1-L3\n" tmp))
      (let ((fill-column 200))
        (gfm-pretty-link-previews--rebuild))
      (let ((ovs (gfm-pretty-link-previews-tests--overlays)))
        (should (= 1 (length ovs)))
        (let* ((ov (car ovs))
               (display (overlay-get ov 'display))
               (abbrev (gfm-pretty-link-previews--abbrev-source-path tmp)))
          (should display)
          (should (string-match-p (regexp-quote (format "%s:1-3" abbrev))
                                  display))
          ;; Overlay range covers just the URL token, not a leading marker.
          (should (= (overlay-start ov) 1))
          (should (= (overlay-end ov) (+ 1 (length (format "%s#L1-L3" tmp))))))))))

(ert-deftest gfm-pretty-link-previews/bare-tilde-path-expands-via-expand-file-name ()
  "Bare `~/path#L<a>-L<b>' resolves via `expand-file-name'."
  (with-temp-buffer
    (insert "~/foo/bar.rs#L1-L3\n")
    (gfm-pretty-link-previews--rebuild)
    (let* ((ovs (gfm-pretty-link-previews-tests--overlays))
           (ov (car ovs))
           (payload (overlay-get ov 'gfm-pretty-link-previews-payload))
           (resolved (nth 4 payload)))
      (should (= 1 (length ovs)))
      (should (equal (expand-file-name "~/foo/bar.rs") resolved)))))

(ert-deftest gfm-pretty-link-previews/bare-relative-path-resolved-against-default-directory ()
  "Bare relative `<dir>/<file>#L<a>-L<b>' resolves against `default-directory'."
  (let ((default-directory "/wt/proj/"))
    (with-temp-buffer
      (setq default-directory "/wt/proj/")
      (insert "modules/auth.rs#L42-L48\n")
      (gfm-pretty-link-previews--rebuild)
      (let* ((ovs (gfm-pretty-link-previews-tests--overlays))
             (ov (car ovs))
             (payload (overlay-get ov 'gfm-pretty-link-previews-payload))
             (resolved (nth 4 payload)))
        (should (= 1 (length ovs)))
        (should (equal "/wt/proj/modules/auth.rs" resolved))))))

(ert-deftest gfm-pretty-link-previews/bare-basename-only-creates-no-overlay ()
  "Basename-only bare reference (no `/' in path) is rejected."
  (with-temp-buffer
    (insert "auth.rs#L1-L5\n")
    (gfm-pretty-link-previews--rebuild)
    (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))))

(ert-deftest gfm-pretty-link-previews/bare-diff-range-creates-lhs-margin-overlay ()
  "Bare `diff:<base>...<head>#<path>' on its own line renders the diff box."
  (gfm-pretty-link-previews-tests--with-fake-call-process "+added\n-removed\n" 0
    (with-temp-buffer
      (insert "diff:abc1234...def5678#main.tf\n")
      (gfm-pretty-link-previews--rebuild)
      (let ((ovs (gfm-pretty-link-previews-tests--overlays)))
        (should (= 1 (length ovs)))
        (let* ((ov (car ovs))
               (display (overlay-get ov 'display))
               (lines (split-string display "\n"))
               (body (cl-subseq lines 1 -1)))
          (should (string-match-p "abc1234\\.\\.\\.def5678" (car lines)))
          (dolist (line body)
            (should (string-match-p "^│[-+ ]" line))))))))

(ert-deftest gfm-pretty-link-previews/bare-list-item-overlay-covers-token-only ()
  "`- <bare-url>' overlay starts at the token, not the `- ' marker."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "- %s#L1-L3\n" tmp))
      (gfm-pretty-link-previews--rebuild)
      (let* ((ovs (gfm-pretty-link-previews-tests--overlays))
             (ov (car ovs))
             (display (overlay-get ov 'display))
             (lines (split-string display "\n"))
             (bot (car (last lines)))
             (body (butlast (cdr lines))))
        (should (= 1 (length ovs)))
        ;; Overlay starts at column 2 (after `- ').
        (should (= 3 (overlay-start ov)))
        ;; Continuation prefix = 2 spaces so `│'/`└' land under the `┌'.
        (should (string-prefix-p "  └" bot))
        (dolist (line body)
          (should (string-prefix-p "  │" line)))))))

(ert-deftest gfm-pretty-link-previews/bare-blockquote-overlay-uses-rail-prefix ()
  "`> <bare-url>' overlay continuation prefix mirrors the blockquote rail."
  (require 'gfm-pretty-blockquotes)
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "> %s#L1-L3\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (let* ((ovs (gfm-pretty-link-previews-tests--overlays))
                 (ov (car ovs))
                 (display (overlay-get ov 'display))
                 (lines (split-string display "\n"))
                 (bot (car (last lines)))
                 (body (butlast (cdr lines))))
            (should (= 1 (length ovs)))
            (should (string-match-p "▌" bot))
            (dolist (line body)
              (should (string-match-p "▌" line))))
        (gfm-pretty-mode -1)))))


;;; Bare-line preformatted-context exclusion

(ert-deftest gfm-pretty-link-previews/bare-in-triple-backtick-fence-is-skipped ()
  "Bare reference inside a ```...``` fence creates no overlay."
  (with-temp-buffer
    (insert "```\n")
    (insert "/abs/path/main.rs#L1-L3\n")
    (insert "```\n")
    (gfm-pretty-link-previews--rebuild)
    (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))))

(ert-deftest gfm-pretty-link-previews/bare-in-four-backtick-fence-is-skipped ()
  "Bare reference inside a ````...```` fence creates no overlay."
  (with-temp-buffer
    (insert "````\n")
    (insert "/abs/path/main.rs#L1-L3\n")
    (insert "````\n")
    (gfm-pretty-link-previews--rebuild)
    (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))))

(ert-deftest gfm-pretty-link-previews/bare-with-four-space-indent-is-skipped ()
  "Bare reference with 4-space leading indent is treated as preformatted."
  (with-temp-buffer
    (insert "    /abs/path/main.rs#L1-L3\n")
    (gfm-pretty-link-previews--rebuild)
    (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))))

(ert-deftest gfm-pretty-link-previews/bare-wrapped-in-inline-code-is-skipped ()
  "Bare reference wrapped in `…` on its own line is left undecorated."
  (with-temp-buffer
    (insert "`/abs/path/main.rs#L1-L3`\n")
    (gfm-pretty-link-previews--rebuild)
    (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))))

(ert-deftest gfm-pretty-link-previews/bare-embedded-in-prose-is-skipped ()
  "Bare reference in a prose sentence is left undecorated."
  (with-temp-buffer
    (insert "See /abs/path/main.rs#L1-L3 for details.\n")
    (gfm-pretty-link-previews--rebuild)
    (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))))

(ert-deftest gfm-pretty-link-previews/bare-toggle-decorator-tears-down-and-rebuilds ()
  "Toggling `link-previews' off then on tears down and recreates the bare overlay."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "%s#L1-L2\n" tmp))
      (gfm-pretty-mode 1)
      (unwind-protect
          (progn
            (should (= 1 (length (gfm-pretty-link-previews-tests--overlays))))
            (gfm-pretty-toggle-decorator 'link-previews)
            (should (= 0 (length (gfm-pretty-link-previews-tests--overlays))))
            (gfm-pretty-toggle-decorator 'link-previews)
            (should (= 1 (length (gfm-pretty-link-previews-tests--overlays)))))
        (gfm-pretty-mode -1)))))

(ert-deftest gfm-pretty-link-previews/bare-and-bracketed-coexist-on-adjacent-lines ()
  "Adjacent bracketed and bare references both produce one overlay each."
  (gfm-pretty-link-previews-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "[foo](%s#L1)\n%s#L1-L2\n" tmp tmp))
      (gfm-pretty-link-previews--rebuild)
      (let ((ovs (gfm-pretty-link-previews-tests--overlays)))
        (should (= 2 (length ovs)))
        ;; The two overlays cover disjoint ranges (no double-decoration).
        (let ((spans (sort (mapcar (lambda (ov)
                                     (cons (overlay-start ov)
                                           (overlay-end ov)))
                                   ovs)
                           (lambda (a b) (< (car a) (car b))))))
          (should (<= (cdr (nth 0 spans)) (car (nth 1 spans)))))))))


(provide 'gfm-pretty-link-previews-tests)

;;; gfm-pretty-link-previews-tests.el ends here
