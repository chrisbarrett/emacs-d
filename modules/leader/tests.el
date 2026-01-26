;;; leader-tests.el --- Tests for leader module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for leader keybindings from spec 007-leader.md

;;; Code:

(require 'ert)

(defvar leader-test--module-dir
  (expand-file-name "modules/leader/" user-emacs-directory)
  "Directory containing the leader module.")

;; Load module init
(let ((init-file (expand-file-name "init.el" leader-test--module-dir)))
  (condition-case err
      (load init-file nil t)
    (error (message "Leader init.el load error: %s" err))))

;; Load lib for function tests
(let ((lib-file (expand-file-name "lib.el" leader-test--module-dir)))
  (condition-case nil
      (load lib-file nil t)
    (error nil)))

;;; Module structure tests

(ert-deftest leader-test-packages-eld-exists ()
  "Module has packages.eld file."
  (let ((file (expand-file-name "packages.eld" leader-test--module-dir)))
    (should (file-exists-p file))))

(ert-deftest leader-test-spec-md-exists ()
  "Module has spec.md file."
  (let ((file (expand-file-name "spec.md" leader-test--module-dir)))
    (should (file-exists-p file))))

(ert-deftest leader-test-init-el-exists ()
  "Module has init.el file."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (should (file-exists-p file))))

(ert-deftest leader-test-lib-el-exists ()
  "Module has lib.el file."
  (let ((file (expand-file-name "lib.el" leader-test--module-dir)))
    (should (file-exists-p file))))

;;; P1: SPC SPC opens consult-buffer

(ert-deftest leader-test-p1-spc-spc-consult-buffer ()
  "P1: SPC SPC is bound to consult-buffer."
  (skip-unless (boundp '+leader-key))
  (let ((binding (lookup-key +leader-key "SPC")))
    (should (or (eq binding 'consult-buffer)
                (and (listp binding)
                     (eq (car binding) 'consult-buffer))))))

;;; P2: SPC s saves the current buffer

(ert-deftest leader-test-p2-spc-s-save-buffer ()
  "P2: SPC s is bound to save-buffer."
  (skip-unless (boundp '+leader-key))
  (let ((binding (lookup-key +leader-key "s")))
    (should (or (eq binding 'save-buffer)
                (and (listp binding)
                     (eq (car binding) 'save-buffer))))))

;;; P3: SPC w - splits vertically

(ert-deftest leader-test-p3-spc-w-minus-vsplit ()
  "P3: SPC w - is bound to +split-window-vertically-dwim."
  (skip-unless (boundp '+leader-key))
  (let* ((w-map (lookup-key +leader-key "w"))
         (binding (when (keymapp w-map)
                    (lookup-key w-map "-"))))
    (should (or (eq binding '+split-window-vertically-dwim)
                (and (listp binding)
                     (eq (car binding) '+split-window-vertically-dwim))))))

;;; P4: SPC w t toggles window dedication

(ert-deftest leader-test-p4-spc-w-t-toggle-dedication ()
  "P4: SPC w t is bound to +toggle-window-dedication."
  (skip-unless (boundp '+leader-key))
  (let* ((w-map (lookup-key +leader-key "w"))
         (binding (when (keymapp w-map)
                    (lookup-key w-map "t"))))
    (should (or (eq binding '+toggle-window-dedication)
                (and (listp binding)
                     (eq (car binding) '+toggle-window-dedication))))))

;;; P5: SPC f o finds sibling file

(ert-deftest leader-test-p5-spc-f-o-sibling-file ()
  "P5: SPC f o is bound to +find-sibling-file."
  (skip-unless (boundp '+leader-key))
  (let* ((f-map (lookup-key +leader-key "f"))
         (binding (when (keymapp f-map)
                    (lookup-key f-map "o"))))
    (should (or (eq binding '+find-sibling-file)
                (and (listp binding)
                     (eq (car binding) '+find-sibling-file))))))

;;; P6: M-m activates +leader-key in emacs state

(ert-deftest leader-test-p6-m-m-leader-key ()
  "P6: M-m is bound to +leader-key."
  (skip-unless (boundp '+leader-key))
  (let ((binding (global-key-binding (kbd "M-m"))))
    (should (eq binding '+leader-key))))

;;; P7: SPC u SPC u produces (16) universal argument

(ert-deftest leader-test-p7-spc-u-chaining ()
  "P7: universal-argument-map has SPC u for chaining."
  (let ((binding (lookup-key universal-argument-map (kbd "SPC u"))))
    (should (eq binding 'universal-argument-more))))

;;; P8: +forward-kill-sexp is defined

(ert-deftest leader-test-p8-forward-kill-sexp-defined ()
  "P8: +forward-kill-sexp function is defined."
  (should (fboundp '+forward-kill-sexp)))

(ert-deftest leader-test-p8-backward-kill-sexp-defined ()
  "P8: +backward-kill-sexp function is defined."
  (should (fboundp '+backward-kill-sexp)))

;;; P9: Side window swap functions (in nav module, verify autoloads)

(ert-deftest leader-test-p9-split-window-commands-autoloaded ()
  "P9: Split window commands are autoloaded."
  ;; These should be autoloaded from nav-lib
  (should (or (fboundp '+split-window-horizontally-dwim)
              ;; Check autoload entry exists in init.el
              (let ((file (expand-file-name "init.el" leader-test--module-dir)))
                (with-temp-buffer
                  (insert-file-contents file)
                  (search-forward "+split-window-horizontally-dwim" nil t))))))

;;; Additional tests for lib.el functions

(ert-deftest leader-test-find-sibling-file-defined ()
  "+find-sibling-file function is defined."
  (should (fboundp '+find-sibling-file)))

(ert-deftest leader-test-find-sibling-file-search-including-nonexisting-defined ()
  "+find-sibling-file-search-including-nonexisting function is defined."
  (should (fboundp '+find-sibling-file-search-including-nonexisting)))

(ert-deftest leader-test-kill-line-defined ()
  "+kill-line function is defined."
  (should (fboundp '+kill-line)))

(ert-deftest leader-test-insert-uuid-defined ()
  "+insert-uuid function is defined."
  (should (fboundp '+insert-uuid)))

;;; Prefix groups verification

(ert-deftest leader-test-prefix-structure ()
  "Leader key has expected prefix groups."
  (skip-unless (boundp '+leader-key))
  (should (keymapp (lookup-key +leader-key ",")))  ; structure
  (should (keymapp (lookup-key +leader-key "a")))  ; apps
  (should (keymapp (lookup-key +leader-key "b")))  ; buffers
  (should (keymapp (lookup-key +leader-key "c")))  ; code/comments
  (should (keymapp (lookup-key +leader-key "e")))  ; errors
  (should (keymapp (lookup-key +leader-key "f")))  ; files
  (should (keymapp (lookup-key +leader-key "g")))  ; git/goto
  (should (keymapp (lookup-key +leader-key "n")))  ; narrowing
  (should (keymapp (lookup-key +leader-key "o")))  ; org
  (should (keymapp (lookup-key +leader-key "t")))  ; toggles
  (should (keymapp (lookup-key +leader-key "w")))) ; windows

;;; Key binding spot checks

(ert-deftest leader-test-spc-x-execute-extended-command ()
  "SPC x is bound to execute-extended-command."
  (skip-unless (boundp '+leader-key))
  (let ((binding (lookup-key +leader-key "x")))
    (should (or (eq binding 'execute-extended-command)
                (and (listp binding)
                     (eq (car binding) 'execute-extended-command))))))

(ert-deftest leader-test-spc-colon-pp-eval ()
  "SPC : is bound to pp-eval-expression."
  (skip-unless (boundp '+leader-key))
  (let ((binding (lookup-key +leader-key ":")))
    (should (or (eq binding 'pp-eval-expression)
                (and (listp binding)
                     (eq (car binding) 'pp-eval-expression))))))

(ert-deftest leader-test-spc-slash-consult-ripgrep ()
  "SPC / is bound to consult-ripgrep."
  (skip-unless (boundp '+leader-key))
  (let ((binding (lookup-key +leader-key "/")))
    (should (or (eq binding 'consult-ripgrep)
                (and (listp binding)
                     (eq (car binding) 'consult-ripgrep))))))

;;; +define-leader-keys macro

(ert-deftest leader-test-define-leader-keys-macro ()
  "+define-leader-keys is defined as a function/macro."
  (should (fboundp '+define-leader-keys)))

;;; init.el content verification for batch mode

(ert-deftest leader-test-init-contains-leader-key-def ()
  "init.el contains +leader-key prefix command definition."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":prefix-command '+leader-key" nil t)))))

(ert-deftest leader-test-init-contains-m-m-binding ()
  "init.el contains M-m global binding."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "(general-def \"M-m\" '+leader-key)" nil t)))))

(ert-deftest leader-test-init-contains-universal-arg-chaining ()
  "init.el contains universal-argument chaining setup."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "universal-argument-map" nil t)))))

;;; lib.el content verification

(ert-deftest leader-test-lib-contains-autoloads ()
  "lib.el contains autoload cookies for public functions."
  (let ((file (expand-file-name "lib.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ";;;###autoload" nil t)))))

(provide 'leader-tests)

;;; leader-tests.el ends here
