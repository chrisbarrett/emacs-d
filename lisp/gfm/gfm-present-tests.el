;;; gfm-present-tests.el --- gfm-present tests -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the gfm-present library.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gfm-present)


;;; Surviving surface

(ert-deftest gfm-present/focus-face-defined ()
  "`gfm-present-focus-face' is defined."
  (should (facep 'gfm-present-focus-face)))

(ert-deftest gfm-present/mode-map-defined ()
  "`gfm-present-mode-map' is a keymap."
  (should (keymapp gfm-present-mode-map)))

(ert-deftest gfm-present/mode-is-buffer-local ()
  "`gfm-present-mode' is a buffer-local minor mode."
  (with-temp-buffer
    (gfm-present-mode 1)
    (should gfm-present-mode)
    (gfm-present-mode -1)
    (should-not gfm-present-mode)))


;;; §3 Heading-narrowed slide model

(defun gfm-present-tests--narrowed-first-line ()
  "Return the text of the first line in the narrowed region."
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(ert-deftest gfm-present/heading-region-encloses-current-h1 ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (goto-char (point-min))
    (search-forward "body B")
    (let ((region (gfm-present--heading-region (point))))
      (should region)
      (should (string-prefix-p "# B"
                               (buffer-substring-no-properties
                                (car region)
                                (min (cdr region) (+ (car region) 3))))))))

(ert-deftest gfm-present/heading-region-treats-fence-as-opaque ()
  (with-temp-buffer
    (insert "# Real\n```\n# Fake\n```\n# Next\n")
    (should (= 2 (length (gfm-present--all-h1-positions))))))

(ert-deftest gfm-present/all-h1-positions-document-order ()
  (with-temp-buffer
    (insert "# A\n# B\n# C\n")
    (let ((positions (gfm-present--all-h1-positions)))
      (should (= 3 (length positions)))
      (should (apply #'< positions)))))

(ert-deftest gfm-present/heading-slug-downcases-and-collapses ()
  (should (equal "auth-tokens-v2"
                 (gfm-present--heading-slug "Auth & Tokens (v2)")))
  (should (equal "setup"
                 (gfm-present--heading-slug "!! Setup !!")))
  (should (equal "hello-world"
                 (gfm-present--heading-slug "Hello   World"))))

(ert-deftest gfm-present/narrow-to-heading-at-narrows ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (search-backward "body B")
    (gfm-present--narrow-to-heading-at (point))
    (should (equal "# B" (gfm-present-tests--narrowed-first-line)))))

(ert-deftest gfm-present/narrow-before-first-h1-falls-back-to-first ()
  (with-temp-buffer
    (insert "preamble\n# A\nbody A\n# B\nbody B\n")
    (goto-char (point-min))
    (gfm-present--narrow-to-heading-at (point))
    (should (equal "# A" (gfm-present-tests--narrowed-first-line)))))

(ert-deftest gfm-present/narrow-after-last-h1-stays-on-last ()
  (with-temp-buffer
    (insert "# A\n# B\n# C\nlast body\n")
    (goto-char (point-max))
    (gfm-present--narrow-to-heading-at (point))
    (should (equal "# C" (gfm-present-tests--narrowed-first-line)))))

(ert-deftest gfm-present/next-slide-advances ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (search-backward "body A")
    (gfm-present--narrow-to-heading-at (point))
    (gfm-present-next-slide)
    (should (equal "# B" (gfm-present-tests--narrowed-first-line)))))

(ert-deftest gfm-present/next-slide-noop-at-last ()
  (with-temp-buffer
    (insert "# A\n# B\n# C\nlast\n")
    (search-backward "last")
    (gfm-present--narrow-to-heading-at (point))
    (let ((before (point-min)))
      (gfm-present-next-slide)
      (should (= before (point-min))))))

(ert-deftest gfm-present/previous-slide-retreats ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\nbody C\n")
    (search-backward "body B")
    (gfm-present--narrow-to-heading-at (point))
    (gfm-present-previous-slide)
    (should (equal "# A" (gfm-present-tests--narrowed-first-line)))))

(ert-deftest gfm-present/previous-slide-noop-at-first ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\n")
    (search-backward "body A")
    (gfm-present--narrow-to-heading-at (point))
    (let ((before (point-min)))
      (gfm-present-previous-slide)
      (should (= before (point-min))))))


;;; §4 Minor mode + entry point

(defmacro gfm-present-tests--with-temp-md (text path-var &rest body)
  "Write TEXT to a fresh markdown temp file, bind its path to PATH-VAR.
Run BODY; afterwards kill any buffer visiting the file and delete the file.

PATH-VAR is bound to the canonical (truename) path so `get-file-buffer'
inside BODY matches regardless of whether `find-file-visit-truename'
is set globally (e.g. by the editing module)."
  (declare (indent 2))
  `(let ((,path-var (file-truename
                     (make-temp-file "pres-test-" nil ".md" ,text))))
     (unwind-protect (progn ,@body)
       (let ((b (get-file-buffer ,path-var)))
         (when (and b (buffer-live-p b))
           (with-current-buffer b (set-buffer-modified-p nil))
           (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
       (when (file-exists-p ,path-var) (delete-file ,path-var)))))

(ert-deftest gfm-present/mode-keymap-binds-required-keys ()
  (should (eq #'gfm-present-next-slide
              (lookup-key gfm-present-mode-map (kbd "C-n"))))
  (should (eq #'gfm-present-next-slide
              (lookup-key gfm-present-mode-map (kbd "C-f"))))
  (should (eq #'gfm-present-previous-slide
              (lookup-key gfm-present-mode-map (kbd "C-p"))))
  (should (eq #'gfm-present-previous-slide
              (lookup-key gfm-present-mode-map (kbd "C-b"))))
  (should (eq #'gfm-present-quit
              (lookup-key gfm-present-mode-map (kbd "C-c q"))))
  (should (eq #'gfm-present-follow-link
              (lookup-key gfm-present-mode-map (kbd "RET")))))

(ert-deftest gfm-present/mode-enable-narrows-to-enclosing-h1 ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\n")
    (search-backward "body B")
    (gfm-present-mode 1)
    (should (equal "# B" (gfm-present-tests--narrowed-first-line)))))

(ert-deftest gfm-present/mode-disable-widens ()
  (with-temp-buffer
    (insert "# A\nbody A\n# B\nbody B\n# C\n")
    (search-backward "body B")
    (gfm-present-mode 1)
    (should (< (- (point-max) (point-min)) (buffer-size)))
    (gfm-present-mode -1)
    (should (= (- (point-max) (point-min)) (buffer-size)))))

(ert-deftest gfm-present/keymap-callable-from-evil-normal ()
  (skip-unless (require 'evil nil t))
  (with-temp-buffer
    (insert "# A\nbody A\n# B\n")
    (goto-char (point-min))
    (when (fboundp 'evil-local-mode) (evil-local-mode 1))
    (when (fboundp 'evil-normal-state) (evil-normal-state))
    (gfm-present-mode 1)
    (should (eq #'gfm-present-next-slide (key-binding (kbd "C-n"))))))

(ert-deftest gfm-present-markdown/opens-and-enables-mode ()
  (gfm-present-tests--with-temp-md "# A\nbody A\n# B\n" path
    (gfm-present-markdown path)
    (let ((b (get-file-buffer path)))
      (should b)
      (with-current-buffer b
        (should gfm-present-mode)
        (should (equal "# A" (gfm-present-tests--narrowed-first-line)))))))

(ert-deftest gfm-present-markdown/missing-file-signals-user-error ()
  (should-error (gfm-present-markdown "/no/such/file/exists.md") :type 'user-error))

(ert-deftest gfm-present-markdown/marks-owned-buffer-on-fresh-open ()
  (gfm-present-tests--with-temp-md "# A\n" path
    (gfm-present-markdown path)
    (with-current-buffer (get-file-buffer path)
      (should gfm-present--owned-buffer))))

(ert-deftest gfm-present-markdown/does-not-mark-when-buffer-pre-existed ()
  (gfm-present-tests--with-temp-md "# A\n" path
    (find-file-noselect path)
    (gfm-present-markdown path)
    (with-current-buffer (get-file-buffer path)
      (should-not gfm-present--owned-buffer))))

(ert-deftest gfm-present/quit-buries-non-owned-buffer ()
  (gfm-present-tests--with-temp-md "# A\n" path
    (find-file-noselect path)
    (gfm-present-markdown path)
    (let ((b (get-file-buffer path)))
      (with-current-buffer b
        (gfm-present-quit)
        (should (buffer-live-p b))
        (should-not gfm-present-mode)
        (should (= (- (point-max) (point-min)) (buffer-size)))))))

(ert-deftest gfm-present/quit-kills-owned-buffer ()
  (gfm-present-tests--with-temp-md "# A\n" path
    (gfm-present-markdown path)
    (let ((b (get-file-buffer path)))
      (with-current-buffer b
        (let ((kill-buffer-query-functions nil)) (gfm-present-quit)))
      (should-not (buffer-live-p b)))))


;;; §5 Heading-text in-doc links

(ert-deftest gfm-present/parse-heading-link-extracts-slug ()
  (should (equal "auth-flow" (gfm-present--parse-heading-link "#auth-flow")))
  (should (equal "section-1" (gfm-present--parse-heading-link "#section-1")))
  (should-not (gfm-present--parse-heading-link "/path"))
  (should-not (gfm-present--parse-heading-link "https://example.com"))
  (should-not (gfm-present--parse-heading-link "diff:a...b"))
  (should-not (gfm-present--parse-heading-link nil)))

(ert-deftest gfm-present/dispatch-heading-link-finds-h2-and-encloses ()
  (with-temp-buffer
    (insert "# First\nbody\n# Auth flow\n## Refresh tokens\nbody\n# Other\n")
    (let ((result (gfm-present--dispatch-heading-link "refresh-tokens")))
      (should (consp result))
      (let ((heading-pos (car result))
            (region (cdr result)))
        (save-excursion
          (goto-char heading-pos)
          (should (looking-at "## Refresh tokens")))
        (save-excursion
          (goto-char (car region))
          (should (looking-at "# Auth flow")))))))

(ert-deftest gfm-present/dispatch-heading-link-finds-h1 ()
  (with-temp-buffer
    (insert "# Token validation\nbody\n# Other\n")
    (let ((result (gfm-present--dispatch-heading-link "token-validation")))
      (should (consp result))
      (save-excursion
        (goto-char (car result))
        (should (looking-at "# Token validation"))))))

(ert-deftest gfm-present/dispatch-heading-link-non-match-returns-pass-through ()
  (with-temp-buffer
    (insert "# Foo\n# Bar\n")
    (should (eq 'pass-through (gfm-present--dispatch-heading-link "missing")))))

(ert-deftest gfm-present/follow-link-heading-narrows-and-pushes-mark ()
  (with-temp-buffer
    (insert "# Top\nSee [refresh](#refresh-tokens) here.\n# Auth flow\n## Refresh tokens\nbody\n")
    (goto-char (point-min))
    (gfm-present-mode 1)
    (search-forward "[refresh]")
    (backward-char 3)
    (let ((click (point)))
      (gfm-present-follow-link)
      (should (equal "# Auth flow" (gfm-present-tests--narrowed-first-line)))
      (should (looking-at "## Refresh tokens"))
      (should (member click (mapcar #'marker-position
                                    (cons (mark-marker) mark-ring)))))))

(ert-deftest gfm-present/follow-link-non-matching-slug-passes-through ()
  (let ((called 0))
    (cl-letf (((symbol-function 'markdown-follow-link-at-point)
               (lambda () (interactive) (cl-incf called))))
      (with-temp-buffer
        (insert "# Top\n[link](#nope) text.\n# Other\n")
        (goto-char (point-min))
        (gfm-present-mode 1)
        (search-forward "[link]")
        (backward-char 2)
        (gfm-present-follow-link)
        (should (= 1 called))))))


;;; §6 Source-range link preview overlay

(defmacro gfm-present-tests--with-temp-source (lines path-var &rest body)
  "Write LINES (a list of strings) to a `.txt' temp file at PATH-VAR.
Run BODY then kill any visiting buffer and delete the file.
`.txt' is used to avoid loading language major modes that may prompt
for tree-sitter grammars and confuse downstream tests."
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

(ert-deftest gfm-present/parse-source-link-with-range ()
  (let ((r (gfm-present--parse-source-link "modules/auth.rs#L42-L67")))
    (should (equal "modules/auth.rs" (car r)))
    (should (= 42 (cadr r)))
    (should (= 67 (cddr r)))))

(ert-deftest gfm-present/parse-source-link-single-line ()
  (let ((r (gfm-present--parse-source-link "foo.el#L7")))
    (should (equal "foo.el" (car r)))
    (should (= 7 (cadr r)))
    (should (= 7 (cddr r)))))

(ert-deftest gfm-present/parse-source-link-rejects-other-forms ()
  (should-not (gfm-present--parse-source-link "modules/auth.rs"))
  (should-not (gfm-present--parse-source-link "https://example.com"))
  (should-not (gfm-present--parse-source-link "#auth-flow"))
  (should-not (gfm-present--parse-source-link "diff:a...b"))
  (should-not (gfm-present--parse-source-link nil)))

(ert-deftest gfm-present/language-from-extension ()
  (should (equal "rust" (gfm-present--language-from-extension "x.rs")))
  (should (equal "elisp" (gfm-present--language-from-extension "x.el")))
  (should (equal "python" (gfm-present--language-from-extension "x.py")))
  (should (equal "markdown" (gfm-present--language-from-extension "x.md")))
  (should (equal "text" (gfm-present--language-from-extension "x.unknown")))
  (should (equal "text" (gfm-present--language-from-extension nil))))

(ert-deftest gfm-present/read-line-range-small ()
  (gfm-present-tests--with-temp-source '("line1" "line2" "line3" "line4") tmp
    (let ((r (gfm-present--read-line-range tmp 2 3)))
      (should (equal '("line2" "line3") (car r)))
      (should (= 0 (cdr r))))))

(ert-deftest gfm-present/read-line-range-truncates-and-reports-extra ()
  (gfm-present-tests--with-temp-source
      (mapcar (lambda (n) (format "line%d" n)) (number-sequence 1 20)) tmp
    (let ((r (gfm-present--read-line-range tmp 1 15)))
      (should (= 10 (length (car r))))
      (should (= 5 (cdr r))))))

(ert-deftest gfm-present/read-line-range-missing-file ()
  (should (eq 'file-not-found
              (gfm-present--read-line-range "/no/such/file" 1 5))))

(ert-deftest gfm-present/read-line-range-invalid-range ()
  (gfm-present-tests--with-temp-source '("line1" "line2") tmp
    (should (eq 'invalid-range (gfm-present--read-line-range tmp 0 5)))
    (should (eq 'invalid-range (gfm-present--read-line-range tmp 5 2)))
    (should (eq 'invalid-range (gfm-present--read-line-range tmp 100 200)))))

(ert-deftest gfm-present/build-preview-fence-no-footer ()
  (let ((s (gfm-present--build-preview-fence
            "rust" "fn" "p" 1 5 "line1\nline2" 0)))
    (should (string-match-p "```rust fn · p:1-5" s))
    (should (string-match-p "line1\nline2" s))
    (should (string-match-p "```\\'" s))
    (should-not (string-match-p "more lines" s))))

(ert-deftest gfm-present/build-preview-fence-with-footer ()
  (let ((s (gfm-present--build-preview-fence
            "rust" "fn" "p" 1 30 "body" 25)))
    (should (string-match-p "\\+25 more lines · click to open" s))))

(ert-deftest gfm-present/source-preview-missing-file-fence ()
  (let ((s (gfm-present--source-preview-fence "/no/such/file" 1 5 "lbl")))
    (should (string-match-p "(file not found: /no/such/file)" s))))

(ert-deftest gfm-present/source-preview-invalid-range-fence ()
  (gfm-present-tests--with-temp-source '("one") tmp
    (let ((s (gfm-present--source-preview-fence tmp 100 200 "lbl")))
      (should (string-match-p "(invalid range)" s)))))

(ert-deftest gfm-present/render-link-previews-creates-overlay ()
  (gfm-present-tests--with-temp-source '("a" "b" "c" "d") tmp
    (with-temp-buffer
      (insert (format "# Slide\nSee [foo](%s#L2-L3) here.\n" tmp))
      (gfm-present--render-link-previews)
      (should (= 1 (length gfm-present--preview-overlays)))
      (let* ((ov (car gfm-present--preview-overlays))
             (display (overlay-get ov 'display)))
        (should display)
        (should (string-match-p "b\nc" display))))))

(ert-deftest gfm-present/render-link-previews-does-not-mutate-buffer ()
  (gfm-present-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1) text\n" tmp))
      (let ((before-text (buffer-string)))
        (gfm-present--render-link-previews)
        (should (equal before-text (buffer-string)))))))

(ert-deftest gfm-present/clear-link-previews-removes-overlays ()
  (gfm-present-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1) text\n" tmp))
      (gfm-present--render-link-previews)
      (should (= 1 (length gfm-present--preview-overlays)))
      (gfm-present--clear-link-previews)
      (should-not gfm-present--preview-overlays))))


;;; §7 Diff-range link preview overlay

(defmacro gfm-present-tests--with-fake-call-process (output exit &rest body)
  "Stub `call-process' to insert OUTPUT and return EXIT during BODY."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'call-process)
              (lambda (_p _i _d _disp &rest _) (insert ,output) ,exit)))
     ,@body))

(ert-deftest gfm-present/parse-diff-link-with-path ()
  (let ((r (gfm-present--parse-diff-link "diff:main...HEAD#auth.rs")))
    (should (equal "main"    (plist-get r :base)))
    (should (equal "HEAD"    (plist-get r :head)))
    (should (equal "auth.rs" (plist-get r :path)))))

(ert-deftest gfm-present/parse-diff-link-without-path ()
  (let ((r (gfm-present--parse-diff-link "diff:HEAD~1...HEAD")))
    (should (equal "HEAD~1" (plist-get r :base)))
    (should (equal "HEAD"   (plist-get r :head)))
    (should-not (plist-get r :path))))

(ert-deftest gfm-present/parse-diff-link-handles-dotted-refs ()
  (let ((r (gfm-present--parse-diff-link "diff:v1.0...v2.0")))
    (should (equal "v1.0" (plist-get r :base)))
    (should (equal "v2.0" (plist-get r :head)))))

(ert-deftest gfm-present/parse-diff-link-rejects-non-diff ()
  (should-not (gfm-present--parse-diff-link "https://example.com"))
  (should-not (gfm-present--parse-diff-link "p.rs#L1"))
  (should-not (gfm-present--parse-diff-link "diff:abc"))
  (should-not (gfm-present--parse-diff-link "diff:..."))
  (should-not (gfm-present--parse-diff-link nil)))

(ert-deftest gfm-present/diff-preview-argv-no-path ()
  (should (equal '("git" "-C" "/wt" "diff" "B...H")
                 (gfm-present--diff-preview-argv "/wt" "B" "H"))))

(ert-deftest gfm-present/diff-preview-argv-with-path ()
  (should (equal '("git" "-C" "/wt" "diff" "B...H" "--" "p.rs")
                 (gfm-present--diff-preview-argv "/wt" "B" "H" "p.rs"))))

(ert-deftest gfm-present/run-diff-preview-empty ()
  (gfm-present-tests--with-fake-call-process "" 0
    (let ((r (gfm-present--run-diff-preview "/wt" "B" "H")))
      (should (eq 'no-changes (plist-get r :status))))))

(ert-deftest gfm-present/run-diff-preview-small ()
  (gfm-present-tests--with-fake-call-process "line1\nline2\nline3\n" 0
    (let ((r (gfm-present--run-diff-preview "/wt" "B" "H")))
      (should (eq 'ok (plist-get r :status)))
      (should (= 0 (plist-get r :extra)))
      (should (string-match-p "line1\nline2\nline3" (plist-get r :body))))))

(ert-deftest gfm-present/run-diff-preview-oversized-truncates ()
  (let ((output (concat (mapconcat (lambda (n) (format "L%d" n))
                                   (number-sequence 1 15) "\n")
                        "\n")))
    (gfm-present-tests--with-fake-call-process output 0
      (let ((r (gfm-present--run-diff-preview "/wt" "B" "H")))
        (should (= 5 (plist-get r :extra)))
        (should (= 10 (length (split-string (plist-get r :body) "\n"))))))))

(ert-deftest gfm-present/diff-preview-fence-empty-says-no-changes ()
  (gfm-present-tests--with-fake-call-process "" 0
    (let ((s (gfm-present--diff-preview-fence "/wt" "B" "H" nil "lbl")))
      (should (string-match-p "(no changes)" s)))))

(ert-deftest gfm-present/diff-preview-fence-error-shows-first-line ()
  (gfm-present-tests--with-fake-call-process "fatal: bad object\nmore\n" 128
    (let ((s (gfm-present--diff-preview-fence "/wt" "B" "H" nil "lbl")))
      (should (string-match-p "(git error: fatal: bad object)" s)))))

(ert-deftest gfm-present/render-link-previews-handles-diff-link ()
  (gfm-present-tests--with-fake-call-process "diff line 1\n" 0
    (with-temp-buffer
      (insert "# Slide\n[change](diff:HEAD~1...HEAD)\n")
      (gfm-present--render-link-previews)
      (should (= 1 (length gfm-present--preview-overlays)))
      (let ((display (overlay-get (car gfm-present--preview-overlays) 'display)))
        (should (string-match-p "diff line 1" display))))))


;;; §8 Preview refresh on slide entry

(ert-deftest gfm-present/mode-enable-rebuilds-previews ()
  (gfm-present-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1-L2)\n" tmp))
      (goto-char (point-min))
      (gfm-present-mode 1)
      (should (= 1 (length gfm-present--preview-overlays))))))

(ert-deftest gfm-present/next-slide-rebuilds-previews ()
  (gfm-present-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# A\nfirst\n# B\n[foo](%s#L1-L2)\n" tmp))
      (goto-char (point-min))
      (gfm-present-mode 1)
      (should (= 0 (length gfm-present--preview-overlays)))
      (gfm-present-next-slide)
      (should (= 1 (length gfm-present--preview-overlays))))))

(ert-deftest gfm-present/previous-slide-rebuilds-previews ()
  (gfm-present-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# A\n[foo](%s#L1-L2)\n# B\nbody\n" tmp))
      (goto-char (point-max))
      (gfm-present-mode 1)
      (should (= 0 (length gfm-present--preview-overlays)))
      (gfm-present-previous-slide)
      (should (= 1 (length gfm-present--preview-overlays))))))

(ert-deftest gfm-present/heading-link-follow-rebuilds-previews ()
  (gfm-present-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# Top\n[jump](#target)\n# Other\n## Target\n[foo](%s#L1-L2)\n"
                      tmp))
      (goto-char (point-min))
      (gfm-present-mode 1)
      (search-forward "[jump]")
      (backward-char 2)
      (gfm-present-follow-link)
      (should (= 1 (length gfm-present--preview-overlays))))))

(ert-deftest gfm-present/no-file-notify-watchers ()
  (let ((src (with-temp-buffer
               (insert-file-contents
                (expand-file-name "lisp/gfm/gfm-present.el"
                                  user-emacs-directory))
               (buffer-string))))
    (should-not (string-match-p "file-notify-add-watch" src))))

(ert-deftest gfm-present/no-idle-timers-for-preview-refresh ()
  (let ((src (with-temp-buffer
               (insert-file-contents
                (expand-file-name "lisp/gfm/gfm-present.el"
                                  user-emacs-directory))
               (buffer-string))))
    (should-not (string-match-p "run-with-idle-timer" src))))


;;; §13 Detached narrowed-source renderer

(ert-deftest gfm-present/render-narrowed-source-narrows-and-readonly ()
  (with-temp-buffer
    (insert "L1\nL2\nL3\nL4\nL5\n")
    (gfm-present--render-narrowed-source (current-buffer) 2 4)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "L2")))
    (should (= 3 (count-lines (point-min) (point-max))))
    (should buffer-read-only)
    (should gfm-present--source-restorer)))

(ert-deftest gfm-present/render-narrowed-source-applies-focus-overlays ()
  (with-temp-buffer
    (insert "L1\nL2\nL3\nL4\nL5\n")
    (gfm-present--render-narrowed-source (current-buffer) 1 5 2 4)
    (let ((focus-ovs (cl-remove-if-not
                      (lambda (o) (eq (overlay-get o 'face) 'gfm-present-focus-face))
                      gfm-present--source-overlays)))
      (should (= 3 (length focus-ovs))))))

(ert-deftest gfm-present/render-narrowed-source-cleans-on-rerender ()
  (with-temp-buffer
    (insert "L1\nL2\nL3\nL4\nL5\n")
    (gfm-present--render-narrowed-source (current-buffer) 1 5 1 2)
    (should (= 2 (length gfm-present--source-overlays)))
    (gfm-present--render-narrowed-source (current-buffer) 1 5 3 5)
    (should (= 3 (length gfm-present--source-overlays)))))

(ert-deftest gfm-present/render-narrowed-source-focus-bounded-to-eol ()
  (with-temp-buffer
    (insert "shortline\nlonger second line\n")
    (gfm-present--render-narrowed-source (current-buffer) 1 2 1 1)
    (let ((ov (car gfm-present--source-overlays)))
      (should ov)
      (should (= (overlay-end ov)
                 (save-excursion
                   (goto-char (overlay-start ov))
                   (line-end-position)))))))

(ert-deftest gfm-present/render-narrowed-source-cleanup-restores-state ()
  (let ((buf (generate-new-buffer "*pres-render-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "L1\nL2\nL3\n")
          (gfm-present--render-narrowed-source buf 1 3)
          (should buffer-read-only)
          (gfm-present--cleanup-source-render)
          (should-not buffer-read-only)
          (should (= (- (point-max) (point-min)) (buffer-size))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest gfm-present/focus-face-no-extend ()
  (should-not (eq t (face-attribute 'gfm-present-focus-face :extend nil t))))


;;; §9 Click escape: source-range link

(defmacro gfm-present-tests--with-source-link-click (lines path link &rest body)
  "Set up a presentation buffer containing LINK pointing into PATH (LINES).
LINK is a string fragment like \"#L2-L3\".  Stubs `pop-to-buffer'
to merely set the current buffer.  Inside BODY: cursor is on the link,
mode is enabled, and BODY can call `gfm-present-follow-link'."
  (declare (indent 3))
  `(gfm-present-tests--with-temp-source ,lines ,path
     (with-temp-buffer
       (insert (format "# Slide\n[foo](%s%s)\n" ,path ,link))
       (goto-char (point-min))
       (gfm-present-mode 1)
       (search-forward "[foo]")
       (backward-char 2)
       (cl-letf (((symbol-function 'pop-to-buffer)
                  (lambda (buffer &rest _) (set-buffer buffer) buffer)))
         ,@body))))

(ert-deftest gfm-present/source-link-click-pushes-mark ()
  (gfm-present-tests--with-temp-source '("a" "b" "c") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1-L2)\n" tmp))
      (goto-char (point-min))
      (gfm-present-mode 1)
      (search-forward "[foo]")
      (backward-char 2)
      (let ((click (point))
            (doc-buffer (current-buffer)))
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (set-buffer buffer) buffer)))
          (gfm-present-follow-link))
        (with-current-buffer doc-buffer
          (should (member click (mapcar #'marker-position
                                        (cons (mark-marker) mark-ring)))))))))

(ert-deftest gfm-present/source-link-click-opens-narrowed-readonly ()
  (gfm-present-tests--with-source-link-click
      '("a" "b" "c" "d" "e") tmp "#L2-L3"
    (gfm-present-follow-link)
    (let ((src (find-buffer-visiting tmp)))
      (should src)
      (with-current-buffer src
        (should buffer-read-only)
        (should-not gfm-present-mode)
        (should (= 2 (- (line-number-at-pos (point-max))
                        (line-number-at-pos (point-min)))))
        (save-excursion
          (goto-char (point-min))
          (should (looking-at "b")))))))

(ert-deftest gfm-present/source-link-click-uses-display-buffer ()
  (let (called)
    (gfm-present-tests--with-temp-source '("a" "b") tmp
      (with-temp-buffer
        (insert (format "# Slide\n[foo](%s#L1)\n" tmp))
        (goto-char (point-min))
        (gfm-present-mode 1)
        (search-forward "[foo]")
        (backward-char 2)
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (buffer &rest _) (setq called t) (set-buffer buffer))))
          (gfm-present-follow-link))
        (should called)))))

(ert-deftest gfm-present/source-link-click-applies-focus-overlay ()
  (gfm-present-tests--with-source-link-click
      '("a" "b" "c" "d") tmp "#L2-L3"
    (gfm-present-follow-link)
    (with-current-buffer (find-buffer-visiting tmp)
      (let ((focus-ovs (cl-remove-if-not
                        (lambda (o) (eq (overlay-get o 'face)
                                        'gfm-present-focus-face))
                        (overlays-in (point-min) (point-max)))))
        (should focus-ovs)
        (should (= 2 (length focus-ovs)))))))

(ert-deftest gfm-present/source-link-click-installs-kill-hook ()
  (gfm-present-tests--with-source-link-click
      '("a" "b") tmp "#L1"
    (gfm-present-follow-link)
    (with-current-buffer (find-buffer-visiting tmp)
      (should (memq #'gfm-present--cleanup-source-render kill-buffer-hook)))))

(ert-deftest gfm-present/source-link-click-leaves-presentation-mode-off ()
  (gfm-present-tests--with-source-link-click
      '("a" "b") tmp "#L1"
    (gfm-present-follow-link)
    (with-current-buffer (find-buffer-visiting tmp)
      (should-not gfm-present-mode))))


;;; §10 Click escape: diff-range link

(defmacro gfm-present-tests--with-magit-stub (captured-var &rest body)
  "Stub `require' to succeed for `magit' and `magit-diff-range' to capture args."
  (declare (indent 1))
  `(let ((,captured-var nil)
         (orig-require (symbol-function 'require)))
     (cl-letf (((symbol-function 'require)
                (lambda (feat &rest args)
                  (if (eq feat 'magit) t (apply orig-require feat args))))
               ((symbol-function 'magit-diff-range)
                (lambda (range &optional _args files)
                  (setq ,captured-var (list range files)))))
       ,@body)))

(ert-deftest gfm-present/diff-link-click-calls-magit-and-pushes-mark ()
  (gfm-present-tests--with-magit-stub captured
    (with-temp-buffer
      (insert "# Slide\n[change](diff:main...HEAD)\n")
      (goto-char (point-min))
      (gfm-present-mode 1)
      (search-forward "[change]")
      (backward-char 2)
      (let ((click (point)))
        (gfm-present-follow-link)
        (should (equal "main...HEAD" (car captured)))
        (should-not (cadr captured))
        (should (member click (mapcar #'marker-position
                                      (cons (mark-marker) mark-ring))))))))

(ert-deftest gfm-present/diff-link-click-passes-path-when-scoped ()
  (gfm-present-tests--with-magit-stub captured
    (with-temp-buffer
      (insert "# Slide\n[change](diff:main...HEAD#auth.rs)\n")
      (goto-char (point-min))
      (gfm-present-mode 1)
      (search-forward "[change]")
      (backward-char 2)
      (gfm-present-follow-link)
      (should (equal "main...HEAD" (car captured)))
      (should (equal '("auth.rs") (cadr captured))))))

(ert-deftest gfm-present/follow-link-https-passes-through ()
  (let ((called 0))
    (cl-letf (((symbol-function 'markdown-follow-link-at-point)
               (lambda () (interactive) (cl-incf called))))
      (with-temp-buffer
        (insert "# Slide\n[ex](https://example.com) text\n")
        (goto-char (point-min))
        (gfm-present-mode 1)
        (search-forward "[ex]")
        (backward-char 2)
        (gfm-present-follow-link)
        (should (= 1 called))))))

(ert-deftest gfm-present/follow-link-plain-path-passes-through ()
  (let ((called 0))
    (cl-letf (((symbol-function 'markdown-follow-link-at-point)
               (lambda () (interactive) (cl-incf called))))
      (with-temp-buffer
        (insert "# Slide\n[file](modules/auth.rs) text\n")
        (goto-char (point-min))
        (gfm-present-mode 1)
        (search-forward "[file]")
        (backward-char 2)
        (gfm-present-follow-link)
        (should (= 1 called))))))

(ert-deftest gfm-present/diff-link-click-no-magit-signals ()
  (let ((orig-require (symbol-function 'require)))
    (cl-letf (((symbol-function 'require)
               (lambda (feat &rest args)
                 (if (eq feat 'magit) nil (apply orig-require feat args)))))
      (with-temp-buffer
        (insert "# Slide\n[change](diff:main...HEAD)\n")
        (goto-char (point-min))
        (gfm-present-mode 1)
        (search-forward "[change]")
        (backward-char 2)
        (should-error (gfm-present-follow-link) :type 'user-error)))))


;;; §12 Document revert resilience

(ert-deftest gfm-present/remember-position-captures-anchor-fields ()
  (with-temp-buffer
    (insert "# Top\nbody\n# Auth flow\nfingerprint here\n# Other\n")
    (goto-char (point-min))
    (gfm-present-mode 1)
    (gfm-present-next-slide)
    (search-forward "fingerprint")
    (backward-char (length "fingerprint"))
    (gfm-present--remember-position)
    (let ((a gfm-present--revert-anchor))
      (should (equal "auth-flow" (plist-get a :slug)))
      (should (= 1 (plist-get a :index)))
      (should (string-prefix-p "fingerprint" (plist-get a :fingerprint)))
      (should (<= (length (plist-get a :fingerprint)) 80))
      (should (numberp (plist-get a :window-start-offset))))))

(ert-deftest gfm-present/remember-position-fingerprint-capped-at-80 ()
  (with-temp-buffer
    (insert "# A\n")
    (insert (make-string 200 ?x))
    (goto-char (point-min))
    (forward-line 1)
    (gfm-present--remember-position)
    (should (= 80 (length (plist-get gfm-present--revert-anchor :fingerprint))))))

(ert-deftest gfm-present/restore-position-by-slug ()
  (with-temp-buffer
    (insert "# Auth flow\nbody\n# Other\nx\n")
    (setq gfm-present--revert-anchor
          (list :slug "auth-flow" :index 0
                :fingerprint nil :window-start-offset 0))
    (gfm-present--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# Auth flow")))))

(ert-deftest gfm-present/restore-position-falls-back-to-index ()
  (with-temp-buffer
    (insert "# Renamed\nbody\n# Two\nx\n")
    (setq gfm-present--revert-anchor
          (list :slug "auth-flow" :index 0
                :fingerprint nil :window-start-offset 0))
    (gfm-present--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# Renamed")))))

(ert-deftest gfm-present/restore-position-clamps-index-to-last ()
  (with-temp-buffer
    (insert "# A\n# B\n")
    (setq gfm-present--revert-anchor
          (list :slug "missing" :index 5
                :fingerprint nil :window-start-offset 0))
    (gfm-present--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# B")))))

(ert-deftest gfm-present/restore-position-falls-back-to-first-when-slug-and-index-fail ()
  (with-temp-buffer
    (insert "# Only\nbody\n")
    (setq gfm-present--revert-anchor
          (list :slug nil :index nil :fingerprint nil :window-start-offset 0))
    (gfm-present--restore-position)
    (save-excursion
      (goto-char (point-min))
      (should (looking-at "# Only")))))

(ert-deftest gfm-present/restore-position-no-h1s-leaves-widened ()
  (with-temp-buffer
    (insert "preamble only\n")
    (setq gfm-present--revert-anchor
          (list :slug "x" :index 0 :fingerprint nil :window-start-offset 0))
    (gfm-present--restore-position)
    (should (= (- (point-max) (point-min)) (buffer-size)))))

(ert-deftest gfm-present/restore-position-finds-fingerprint ()
  (with-temp-buffer
    (insert "# Top\nbefore\nUNIQUEFINGERPRINT\nafter\n")
    (setq gfm-present--revert-anchor
          (list :slug "top" :index 0
                :fingerprint "UNIQUEFINGERPRINT"
                :window-start-offset 0))
    (gfm-present--restore-position)
    (should (looking-at "UNIQUEFINGERPRINT"))))

(ert-deftest gfm-present/restore-position-rebuilds-previews ()
  (gfm-present-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format "# Slide\n[foo](%s#L1)\n" tmp))
      (gfm-present-mode 1)
      (setq gfm-present--revert-anchor
            (list :slug "slide" :index 0 :fingerprint nil :window-start-offset 0))
      (gfm-present--clear-link-previews)
      (gfm-present--restore-position)
      (should (= 1 (length gfm-present--preview-overlays))))))

(ert-deftest gfm-present/mode-installs-and-removes-revert-hooks ()
  (with-temp-buffer
    (gfm-present-mode 1)
    (should (memq #'gfm-present--remember-position before-revert-hook))
    (should (memq #'gfm-present--restore-position after-revert-hook))
    (gfm-present-mode -1)
    (should-not (memq #'gfm-present--remember-position before-revert-hook))
    (should-not (memq #'gfm-present--restore-position after-revert-hook))))

(ert-deftest gfm-present/revert-buffer-preserves-mode-and-narrowing ()
  "End-to-end: real `revert-buffer' must keep mode on and re-narrow to the
same slug.  `kill-all-local-variables' otherwise clears the anchor, the
buffer-local revert hooks, and the minor-mode flag before
`after-revert-hook' runs."
  (gfm-present-tests--with-temp-md
      "# One\nbody one\n# Two\nfingerprint-two\n# Three\nbody three\n" tmp
    (find-file tmp)
    (unwind-protect
        (progn
          (gfm-present-mode 1)
          (gfm-present-next-slide)
          (search-forward "fingerprint-two")
          (backward-char (length "fingerprint-two"))
          (let ((coding-system-for-write 'utf-8))
            (write-region
             "# One\nbody one\n# Two\nfingerprint-two changed\n# Three\nbody three\n"
             nil tmp))
          (revert-buffer t t)
          (should gfm-present-mode)
          (should (buffer-narrowed-p))
          (save-excursion
            (goto-char (point-min))
            (should (looking-at "# Two"))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer)))))


;;; §5b Pretty-links anchor-jump integration

(ert-deftest gfm-present/anchor-jump-narrows-to-target-h1 ()
  "RET on a pretty-links anchor lands narrowed to the target's slide region."
  (require 'gfm-pretty-links)
  (with-temp-buffer
    (insert "# Slide one\n[jump](#filter-shape-change) here.\n\n"
            "# Filter shape change\ndetails\n")
    (goto-char (point-min))
    (gfm-present-mode 1)
    (should (equal "# Slide one" (gfm-present-tests--narrowed-first-line)))
    (gfm-pretty-links--jump-to-anchor "#filter-shape-change")
    (should (buffer-narrowed-p))
    (should (equal "# Filter shape change"
                   (gfm-present-tests--narrowed-first-line)))
    (should (looking-at "# Filter shape change"))))

(ert-deftest gfm-present/anchor-jump-rebuilds-link-previews ()
  "After a pretty-links anchor jump, the new slide's previews refresh."
  (require 'gfm-pretty-links)
  (gfm-present-tests--with-temp-source '("a" "b") tmp
    (with-temp-buffer
      (insert (format
               "# Slide one\n[jump](#target)\n\n# Target\n[foo](%s#L1-L2)\n"
               tmp))
      (goto-char (point-min))
      (gfm-present-mode 1)
      (should (= 0 (length gfm-present--preview-overlays)))
      (gfm-pretty-links--jump-to-anchor "#target")
      (should (= 1 (length gfm-present--preview-overlays))))))

(ert-deftest gfm-present/registers-anchor-jump-hook-on-enable ()
  "Mode enable adds `gfm-present--after-anchor-jump' to the buffer-local hook."
  (require 'gfm-pretty-links)
  (with-temp-buffer
    (gfm-present-mode 1)
    (should (memq #'gfm-present--after-anchor-jump
                  gfm-pretty-links-after-anchor-jump-functions))))

(ert-deftest gfm-present/removes-anchor-jump-hook-on-disable ()
  "Mode disable removes the present-mode subscriber from the hook."
  (require 'gfm-pretty-links)
  (with-temp-buffer
    (gfm-present-mode 1)
    (gfm-present-mode -1)
    (should-not (memq #'gfm-present--after-anchor-jump
                      gfm-pretty-links-after-anchor-jump-functions))))

(ert-deftest gfm-present/anchor-jump-hook-is-buffer-local ()
  "The anchor-jump subscriber is added with LOCAL=t, not globally."
  (require 'gfm-pretty-links)
  (let ((global-before (default-value
                        'gfm-pretty-links-after-anchor-jump-functions)))
    (with-temp-buffer
      (gfm-present-mode 1)
      (should (equal global-before
                     (default-value
                      'gfm-pretty-links-after-anchor-jump-functions))))))


;;; §14 Module-level cleanup

(defun gfm-present-tests--read-lib ()
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "lisp/gfm/gfm-present.el"
                       user-emacs-directory))
    (buffer-string)))

(ert-deftest gfm-present/lib-el-exports-public-symbols ()
  (should (fboundp 'gfm-present-markdown))
  (should (fboundp 'gfm-present-mode))
  (should (fboundp 'gfm-present-next-slide))
  (should (fboundp 'gfm-present-previous-slide))
  (should (fboundp 'gfm-present-quit))
  (should (fboundp 'gfm-present-follow-link))
  (should (facep 'gfm-present-focus-face)))

(ert-deftest gfm-present/no-references-to-deleted-symbols ()
  (let ((src (gfm-present-tests--read-lib))
        (deleted '("gfm-present--sessions"
                   "gfm-present--effect-shell"
                   "gfm-present--effect-elisp"
                   "gfm-present--run-effects"
                   "gfm-present--register-session"
                   "gfm-present--get-session"
                   "gfm-present--make-key"
                   "gfm-present--deck-push"
                   "gfm-present--deck-replace"
                   "gfm-present--deck-truncate"
                   "gfm-present--deck-goto"
                   "gfm-present--render-current"
                   "gfm-present--render-slide"
                   "gfm-present--dispatch-slide"
                   "gfm-present--validate-slide"
                   "gfm-present--coerce-slide"
                   "gfm-present--emit-nav-channel"
                   "gfm-present--inject-channel-capability"
                   "gfm-present--register-channel-capability")))
    (dolist (sym deleted)
      (should-not (string-match-p (regexp-quote sym) src)))))

(provide 'gfm-present-tests)
;;; gfm-present-tests.el ends here
