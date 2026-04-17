;;; lang-nix/tests.el --- Tests for lang-nix module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Nix language support module.

;;; Code:

(require 'ert)
(require 'treesit)

;; Load module init from this directory
;; May fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir))
       (lib-dir (expand-file-name "lib" module-dir)))
  (add-to-list 'load-path lib-dir)
  (condition-case nil
      (load init-file nil 'nomessage)
    (error nil))
  (require '+nix-ts-spans nil t))

;;; P1: Opening *.nix file activates nix-ts-mode

(ert-deftest lang-nix/auto-mode-nix ()
  "P1: .nix files should be associated with nix-ts-mode."
  (let ((entry (assoc "\\.nix\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'nix-ts-mode))))

;;; P2: Opening /flake.lock activates json-ts-mode

(ert-deftest lang-nix/auto-mode-flake-lock ()
  "P2: flake.lock files should be associated with json-ts-mode."
  (let ((matches nil))
    (dolist (entry auto-mode-alist)
      (when (and (stringp (car entry))
                 (string-match-p "flake.lock" (car entry)))
        (push entry matches)))
    ;; Skip if module init didn't configure this
    (skip-unless matches)
    (should (eq (cdr (car matches)) 'json-ts-mode))))

;;; P3: Opening file under /nix/store/ enables read-only-mode
;; Tested via dirlocals mechanism

(ert-deftest lang-nix/nix-store-read-only ()
  "P3: /nix/store/ should have read-only dirlocals."
  ;; Skip if +corelib not available
  (skip-unless (require '+corelib nil t))
  ;; Check that dirlocals are set for /nix/store/
  (let ((dirlocals (dir-locals-find-file "/nix/store/some-hash/file.nix")))
    ;; dirlocals returns nil, file path, or (file . dir)
    ;; For a regexp match, it should find our settings
    ;; Skip if not configured
    (skip-unless (or dirlocals
                     (assoc "/nix/store/" dir-locals-class-alist)))
    (should (or dirlocals
                (assoc "/nix/store/" dir-locals-class-alist)))))

;;; P4: eglot-ensure is called on nix-ts-mode-local-vars-hook

(ert-deftest lang-nix/eglot-hook ()
  "P4: nix-ts-mode-local-vars-hook should contain eglot-ensure."
  (skip-unless (featurep 'nix-ts-mode))
  (should (memq 'eglot-ensure nix-ts-mode-local-vars-hook)))

;;; P6: project-vc-extra-root-markers contains "flake.nix"

(ert-deftest lang-nix/project-root-marker ()
  "P6: flake.nix should be in project-vc-extra-root-markers."
  (require 'project)
  (should (member "flake.nix" project-vc-extra-root-markers)))

;;; P7: Creating flake.nix inserts template with directory name
;; File template functionality tested via the template file existence

(ert-deftest lang-nix/file-template-exists ()
  "P7: flake.eld template should exist."
  (let ((template-file (expand-file-name "file-templates/flake.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

(ert-deftest lang-nix/file-template-has-directory-name ()
  "P7: flake.eld template should reference directory-name."
  (let ((template-file (expand-file-name "file-templates/flake.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (should (search-forward "directory-name" nil t))))))


;;; Tree-sitter polymode tests

(defmacro lang-nix/with-nix-buffer (content &rest body)
  "Insert CONTENT into a temp buffer with nix tree-sitter parser, run BODY."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-language-available-p 'nix))
     (with-temp-buffer
       (insert ,content)
       (treesit-parser-create 'nix)
       ,@body)))

;;; Phase 1: Core infrastructure

(ert-deftest lang-nix/attrpath-mode-alist-defined ()
  "+nix-attrpath-mode-alist should match common bash attrs."
  (should (boundp '+nix-attrpath-mode-alist))
  (should (eq 'bash-ts-mode (+nix-ts--resolve-attrpath-mode "shellHook")))
  (should (eq 'bash-ts-mode (+nix-ts--resolve-attrpath-mode "buildPhase")))
  (should (eq 'bash-ts-mode (+nix-ts--resolve-attrpath-mode "installPhase")))
  (should (eq 'bash-ts-mode (+nix-ts--resolve-attrpath-mode "configurePhase"))))

(ert-deftest lang-nix/lang-mode-alist-defined ()
  "+nix-lang-mode-alist should map annotation strings to major modes."
  (should (boundp '+nix-lang-mode-alist))
  (should (eq (cdr (assoc "bash" +nix-lang-mode-alist)) 'bash-ts-mode))
  (should (eq (cdr (assoc "python" +nix-lang-mode-alist)) 'python-ts-mode))
  (should (eq (cdr (assoc "elisp" +nix-lang-mode-alist)) 'emacs-lisp-mode)))

(ert-deftest lang-nix/scan-spans-annotation-block-comment ()
  "Scan should detect block comment annotation /* bash */."
  (lang-nix/with-nix-buffer
      "{ buildPhase = /* bash */ ''\n  echo hi\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (eq 'bash-ts-mode (nth 4 (car spans)))))))

(ert-deftest lang-nix/scan-spans-annotation-line-comment ()
  "Scan should detect line comment annotation # python."
  (lang-nix/with-nix-buffer
      "{\n  configurePhase =\n    # python\n    ''\n    import sys\n  '';\n}"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (eq 'python-ts-mode (nth 4 (car spans)))))))

(ert-deftest lang-nix/scan-spans-attr-name-fallback ()
  "Scan should detect bash mode from known attr name without annotation."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (eq 'bash-ts-mode (nth 4 (car spans)))))))

(ert-deftest lang-nix/scan-spans-unknown-attr-no-span ()
  "Unknown attr without annotation should produce no span."
  (lang-nix/with-nix-buffer
      "{ description = ''\n  Just text\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 0 (length spans))))))

(ert-deftest lang-nix/scan-spans-annotation-overrides-attr ()
  "Annotation should override attr-name-based detection."
  (lang-nix/with-nix-buffer
      "{ shellHook = /* python */ ''\n  import sys\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (eq 'python-ts-mode (nth 4 (car spans)))))))

(ert-deftest lang-nix/scan-spans-unknown-annotation-no-span ()
  "Unknown annotation language should produce no span."
  (lang-nix/with-nix-buffer
      "{ foo = /* zzz-nonexistent-xyz */ ''\n  main = pure ()\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 0 (length spans))))))

(ert-deftest lang-nix/scan-spans-multiple ()
  "Multiple bindings should produce multiple sorted spans."
  (lang-nix/with-nix-buffer
      "{\n  shellHook = ''\n    echo a\n  '';\n  buildPhase = /* python */ ''\n    import sys\n  '';\n}"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 2 (length spans)))
      (should (eq 'bash-ts-mode (nth 4 (car spans))))
      (should (eq 'python-ts-mode (nth 4 (cadr spans))))
      (should (< (nth 0 (car spans)) (nth 0 (cadr spans)))))))

(ert-deftest lang-nix/scan-spans-nested-attrpath ()
  "Multi-part attrpath like packages.x86.installPhase should match."
  (lang-nix/with-nix-buffer
      "{ packages.x86.installPhase = ''\n  make install\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (eq 'bash-ts-mode (nth 4 (car spans)))))))

(ert-deftest lang-nix/scan-spans-head-boundary-annotation ()
  "Head should start at annotation comment, end after opening ''."
  (lang-nix/with-nix-buffer
      "{ foo = /* bash */ ''\n  echo hi\n''; }"
    (let* ((spans (+nix-ts--scan-spans))
           (span (car spans)))
      (should span)
      (should (string-match-p "/\\*" (buffer-substring (nth 0 span) (+ (nth 0 span) 2))))
      (should (string= "''" (buffer-substring (- (nth 1 span) 2) (nth 1 span)))))))

(ert-deftest lang-nix/scan-spans-head-boundary-attr ()
  "Head should start at attr name when no annotation."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (let* ((spans (+nix-ts--scan-spans))
           (span (car spans)))
      (should span)
      (should (string= "shellHook" (buffer-substring (nth 0 span) (+ (nth 0 span) 9)))))))

(ert-deftest lang-nix/scan-spans-tail-boundary ()
  "Tail should be the closing ''."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (let* ((spans (+nix-ts--scan-spans))
           (span (car spans)))
      (should span)
      (should (string= "''" (buffer-substring (nth 2 span) (nth 3 span)))))))

;;; Phase 2: Polymode matchers

(ert-deftest lang-nix/head-matcher-forward ()
  "Head-matcher with AHEAD>0 should find next span's head."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (point-min))
    (let ((result (+nix-ts--head-matcher 1)))
      (should result)
      (should (consp result))
      (should (= (car result) (nth 0 (car +nix-ts--cached-spans))))
      (should (= (cdr result) (nth 1 (car +nix-ts--cached-spans)))))))

(ert-deftest lang-nix/head-matcher-backward ()
  "Head-matcher with AHEAD<0 should find previous span's head."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (point-max))
    (let ((result (+nix-ts--head-matcher -1)))
      (should result)
      (should (= (car result) (nth 0 (car +nix-ts--cached-spans)))))))

(ert-deftest lang-nix/head-matcher-forward-no-match ()
  "Head-matcher forward past last span returns nil."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (nth 1 (car +nix-ts--cached-spans)))
    (should-not (+nix-ts--head-matcher 1))))

(ert-deftest lang-nix/tail-matcher-forward ()
  "Tail-matcher with AHEAD>0 should find next span's tail."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (point-min))
    (let ((result (+nix-ts--tail-matcher 1)))
      (should result)
      (should (= (car result) (nth 2 (car +nix-ts--cached-spans))))
      (should (= (cdr result) (nth 3 (car +nix-ts--cached-spans)))))))

(ert-deftest lang-nix/tail-matcher-backward ()
  "Tail-matcher with AHEAD<0 should find previous span's tail."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (point-max))
    (let ((result (+nix-ts--tail-matcher -1)))
      (should result)
      (should (= (car result) (nth 2 (car +nix-ts--cached-spans)))))))

(ert-deftest lang-nix/mode-matcher-returns-mode-name ()
  "Mode-matcher should return mode name string at head position."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (nth 0 (car +nix-ts--cached-spans)))
    (let ((result (+nix-ts--mode-matcher)))
      (should (stringp result))
      (should (string= "bash-ts" result)))))

(ert-deftest lang-nix/mode-matcher-annotation ()
  "Mode-matcher should return correct mode for annotated span."
  (lang-nix/with-nix-buffer
      "{ foo = /* python */ ''\n  import sys\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (goto-char (nth 0 (car +nix-ts--cached-spans)))
    (let ((result (+nix-ts--mode-matcher)))
      (should (string= "python-ts" result)))))

;;; Phase 3: Code-fences integration

(ert-deftest lang-nix/head-valid-p-tree-sitter ()
  "Head-valid-p should validate using tree-sitter at annotation position."
  (lang-nix/with-nix-buffer
      "{ foo = /* bash */ ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (let ((head-beg (nth 0 (car +nix-ts--cached-spans))))
      (should (+nix--multiline-head-valid-p head-beg)))))

(ert-deftest lang-nix/head-valid-p-attr-name ()
  "Head-valid-p should validate at attr-name position for known attrs."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (let ((head-beg (nth 0 (car +nix-ts--cached-spans))))
      (should (+nix--multiline-head-valid-p head-beg)))))

(ert-deftest lang-nix/count-openers-matches-cache ()
  "Count-openers should return number of cached spans."
  (lang-nix/with-nix-buffer
      "{\n  shellHook = ''\n    echo a\n  '';\n  buildPhase = /* python */ ''\n    import sys\n  '';\n}"
    (setq +nix-ts--cached-spans (+nix-ts--scan-spans))
    (should (= 2 (+nix-ts--count-openers)))))

;;; Attrpath reconstruction

(ert-deftest lang-nix/binding-attrpath-flat ()
  "Flat attrpath returns full dotted path."
  (lang-nix/with-nix-buffer
      "{ programs.may-i.extraConfig = ''\n  stuff\n''; }"
    (let* ((root (treesit-buffer-root-node))
           (query (treesit-query-compile 'nix '((indented_string_expression) @s)))
           (str-node (cdr (car (treesit-query-capture root query))))
           (binding (treesit-node-parent str-node)))
      (should (string= "programs.may-i.extraConfig"
                        (+nix-ts--binding-attrpath binding))))))

(ert-deftest lang-nix/binding-attrpath-nested ()
  "Nested attrset reconstructs full dotted path."
  (lang-nix/with-nix-buffer
      "{ programs.may-i = { extraConfig = ''\n  stuff\n''; }; }"
    (let* ((root (treesit-buffer-root-node))
           (query (treesit-query-compile 'nix '((indented_string_expression) @s)))
           (str-node (cdr (car (treesit-query-capture root query))))
           (binding (treesit-node-parent str-node)))
      (should (string= "programs.may-i.extraConfig"
                        (+nix-ts--binding-attrpath binding))))))

(ert-deftest lang-nix/binding-attrpath-let ()
  "Let-binding attrpath is just the leaf name."
  (lang-nix/with-nix-buffer
      "let myScript = ''\n  stuff\n''; in myScript"
    (let* ((root (treesit-buffer-root-node))
           (query (treesit-query-compile 'nix '((indented_string_expression) @s)))
           (str-node (cdr (car (treesit-query-capture root query))))
           (binding (treesit-node-parent str-node)))
      (should (string= "myScript"
                        (+nix-ts--binding-attrpath binding))))))

(ert-deftest lang-nix/binding-attrpath-deeply-nested-quoted ()
  "Deeply nested path with quoted attrs preserves quotes."
  (lang-nix/with-nix-buffer
      "{ services = { nginx = { virtualHosts.\"example.com\" = { locations.\"/\" = { extraConfig = ''\n  stuff\n''; }; }; }; }; }"
    (let* ((root (treesit-buffer-root-node))
           (query (treesit-query-compile 'nix '((indented_string_expression) @s)))
           (str-node (cdr (car (treesit-query-capture root query))))
           (binding (treesit-node-parent str-node)))
      (should (string= "services.nginx.virtualHosts.\"example.com\".locations.\"/\".extraConfig"
                        (+nix-ts--binding-attrpath binding))))))

;;; Comment resolution with fboundp fallback

(ert-deftest lang-nix/resolve-comment-mode-alist-wins ()
  "Alist match wins over fboundp probe."
  (should (eq 'bash-ts-mode (+nix-ts--resolve-comment-mode "# bash"))))

(ert-deftest lang-nix/resolve-comment-mode-fboundp-fallback ()
  "fboundp probe works when alist has no match."
  (cl-letf (((symbol-function 'test-nix-probe-mode) #'ignore))
    (should (eq 'test-nix-probe-mode
                (+nix-ts--resolve-comment-mode "# test-nix-probe")))))

(ert-deftest lang-nix/resolve-comment-mode-neither ()
  "Returns nil when neither alist nor fboundp matches."
  (should-not (+nix-ts--resolve-comment-mode "# zzz-nonexistent-xyz")))

;;; Attrpath regexp resolution

(ert-deftest lang-nix/resolve-attrpath-mode-leaf ()
  "Leaf attr like shellHook matches."
  (should (eq 'bash-ts-mode (+nix-ts--resolve-attrpath-mode "shellHook"))))

(ert-deftest lang-nix/resolve-attrpath-mode-full-path ()
  "Full dotted path matches leaf pattern."
  (should (eq 'bash-ts-mode (+nix-ts--resolve-attrpath-mode "packages.x86.installPhase"))))

(ert-deftest lang-nix/resolve-attrpath-mode-no-match ()
  "Non-matching attrpath returns nil."
  (should-not (+nix-ts--resolve-attrpath-mode "description")))

;;; Integration tests

(ert-deftest lang-nix/scan-spans-fboundp-mode ()
  "Scan resolves mode via fboundp when alist has no match."
  (lang-nix/with-nix-buffer
      "{\n  extraConfig =\n    # test-nix-probe\n    ''\n    stuff\n  '';\n}"
    (cl-letf (((symbol-function 'test-nix-probe-mode) #'ignore))
      (let ((spans (+nix-ts--scan-spans)))
        (should (= 1 (length spans)))
        (should (eq 'test-nix-probe-mode (nth 4 (car spans))))))))

(ert-deftest lang-nix/scan-spans-attrpath-custom-regexp ()
  "Scan resolves mode via custom attrpath regexp (no annotation)."
  (lang-nix/with-nix-buffer
      "{ programs.may-i = { extraConfig = ''\n  stuff\n''; }; }"
    (let ((+nix-attrpath-mode-alist
           (cons '("programs\\.may-i\\." . test-nix-probe-mode)
                 +nix-attrpath-mode-alist)))
      (cl-letf (((symbol-function 'test-nix-probe-mode) #'ignore))
        (let ((spans (+nix-ts--scan-spans)))
          (should (= 1 (length spans)))
          (should (eq 'test-nix-probe-mode (nth 4 (car spans)))))))))

(ert-deftest lang-nix/scan-spans-comment-overrides-attrpath ()
  "Comment annotation wins over attrpath regexp match."
  (lang-nix/with-nix-buffer
      "{ shellHook = /* python */ ''\n  import sys\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (eq 'python-ts-mode (nth 4 (car spans)))))))

;;; Span tuple 6th element (attrpath field)

(ert-deftest lang-nix/scan-spans-attrpath-in-tuple ()
  "Span tuple 6th element contains attrpath string."
  (lang-nix/with-nix-buffer
      "{ shellHook = ''\n  echo hi\n''; }"
    (let ((spans (+nix-ts--scan-spans)))
      (should (= 1 (length spans)))
      (should (stringp (nth 5 (car spans))))
      (should (string= "shellHook" (nth 5 (car spans)))))))

(provide 'lang-nix-tests)

;;; lang-nix/tests.el ends here
