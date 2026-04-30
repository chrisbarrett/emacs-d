;;; lang-markdown/init.el --- Markdown language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Markdown and GitHub Flavored Markdown (GFM) editing support with visual
;; enhancements, smart formatting, and callout highlighting.

;;; Code:

(require '+autoloads)
(require '+corelib)

;;; Mode configuration

(use-package markdown-mode
  :commands (gfm-mode)
  :init
  ;; Directly associate markdown files with gfm-mode for GitHub compatibility
  ;; Use add-to-list to prepend, overriding built-in markdown-ts-mode-maybe
  (add-to-list 'auto-mode-alist `(,(rx "." (or "md" "markdown" "mkd" "mdown" "mkdn") eos) . gfm-mode))
  ;; Associate /prompt files with gfm-mode (Claude prompt files)
  (add-to-list 'auto-mode-alist `(,(rx "/prompt" eos) . gfm-mode))
  ;; Remap any remaining markdown-mode calls to gfm-mode
  (alist-set! major-mode-remap-alist 'markdown-mode 'gfm-mode)

  :general-config
  (:keymaps 'markdown-mode-map "C-c f" #'markdown-insert-footnote)
  (:keymaps 'markdown-mode-map :states 'normal "SPC n s" #'markdown-narrow-to-subtree)
  (:states 'insert
   :keymaps '(markdown-mode-map gfm-mode-map)
   "TAB" #'+markdown-tab-dwim)

  :hook
  (gfm-mode-hook . visual-line-mode)
  (gfm-mode-hook . +markdown-fontify-gfm-callouts)
  (gfm-mode-hook . gfm-callouts-mode)
  (gfm-mode-hook . gfm-code-fences-mode)
  (gfm-mode-hook . gfm-tables-mode)

  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls t)
  ;; Map GitHub-recognised language tags to major-mode symbols. Used by
  ;; `markdown-get-lang-mode' for native fontification, and (without the
  ;; `fboundp' filter) by `gfm-code-fences--lang-mode' for icon lookup.
  (markdown-code-lang-modes
   '(;; Defaults shipped by markdown-mode that we want to keep.
     ("asymptote"     . asy-mode)
     ("calc"          . fundamental-mode)
     ("ditaa"         . artist-mode)
     ("dot"           . fundamental-mode)
     ("screen"        . shell-script-mode)
     ("sqlite"        . sql-mode)
     ;; C-family
     ("c"             . c-mode)
     ("c#"            . csharp-mode)
     ("cs"            . csharp-mode)
     ("csharp"        . csharp-mode)
     ("c++"           . c++-mode)
     ("cpp"           . c++-mode)
     ("objc"          . objc-mode)
     ("objective-c"   . objc-mode)
     ("objectivec"    . objc-mode)
     ;; Scripting
     ("bash"          . sh-mode)
     ("console"       . sh-mode)
     ("sh"            . sh-mode)
     ("shell"         . sh-mode)
     ("shell-session" . sh-mode)
     ("shellsession"  . sh-mode)
     ("zsh"           . sh-mode)
     ("fish"          . fish-mode)
     ("powershell"    . powershell-mode)
     ("ps1"           . powershell-mode)
     ("pwsh"          . powershell-mode)
     ("bat"           . bat-mode)
     ("batch"         . bat-mode)
     ("nu"            . nushell-mode)
     ("nushell"       . nushell-mode)
     ;; Web
     ("html"          . html-mode)
     ("xml"           . nxml-mode)
     ("css"           . css-mode)
     ("scss"          . scss-mode)
     ("sass"          . sass-mode)
     ("less"          . less-css-mode)
     ("stylus"        . stylus-mode)
     ("vue"           . vue-mode)
     ("svelte"        . svelte-mode)
     ("haml"          . haml-mode)
     ("pug"           . pug-mode)
     ("jade"          . jade-mode)
     ;; JS/TS family
     ("javascript"    . js-mode)
     ("js"            . js-mode)
     ("jsx"           . js-jsx-mode)
     ("typescript"    . typescript-mode)
     ("ts"            . typescript-mode)
     ("tsx"           . tsx-ts-mode)
     ("coffee"        . coffee-mode)
     ("coffeescript"  . coffee-mode)
     ;; Systems / general-purpose
     ("go"            . go-mode)
     ("golang"        . go-mode)
     ("rust"          . rust-mode)
     ("rs"            . rust-mode)
     ("ron"           . ron-mode)
     ("zig"           . zig-mode)
     ("nim"           . nim-mode)
     ("crystal"       . crystal-mode)
     ("d"             . d-mode)
     ("ada"           . ada-mode)
     ("verilog"       . verilog-mode)
     ;; JVM
     ("java"          . java-mode)
     ("kotlin"        . kotlin-mode)
     ("kt"            . kotlin-mode)
     ("kts"           . kotlin-mode)
     ("scala"         . scala-mode)
     ("groovy"        . groovy-mode)
     ("clojure"       . clojure-mode)
     ("clj"           . clojure-mode)
     ("clojurescript" . clojurescript-mode)
     ("cljs"          . clojurescript-mode)
     ("cljc"          . clojurec-mode)
     ;; Functional / Lisp
     ("emacs-lisp"    . emacs-lisp-mode)
     ("elisp"         . emacs-lisp-mode)
     ("el"            . emacs-lisp-mode)
     ("common-lisp"   . lisp-mode)
     ("lisp"          . lisp-mode)
     ("scheme"        . scheme-mode)
     ("racket"        . scheme-mode)
     ("haskell"       . haskell-mode)
     ("hs"            . haskell-mode)
     ("ocaml"         . tuareg-mode)
     ("ml"            . tuareg-mode)
     ("elm"           . elm-mode)
     ("erlang"        . erlang-mode)
     ("erl"           . erlang-mode)
     ("elixir"        . elixir-mode)
     ("ex"            . elixir-mode)
     ("exs"           . elixir-mode)
     ("purescript"    . purescript-mode)
     ("fsharp"        . fsharp-mode)
     ("f#"            . fsharp-mode)
     ;; Dynamic
     ("python"        . python-mode)
     ("py"            . python-mode)
     ("ruby"          . ruby-mode)
     ("rb"            . ruby-mode)
     ("perl"          . perl-mode)
     ("pl"            . perl-mode)
     ("php"           . php-mode)
     ("lua"           . lua-mode)
     ("r"             . ess-r-mode)
     ("matlab"        . matlab-mode)
     ("dart"          . dart-mode)
     ("swift"         . swift-mode)
     ("gdscript"      . gdscript-mode)
     ;; Data / config
     ("json"          . json-mode)
     ("jsonc"         . jsonc-mode)
     ("yaml"          . yaml-mode)
     ("yml"           . yaml-mode)
     ("toml"          . toml-mode)
     ("csv"           . csv-mode)
     ("graphql"       . graphql-mode)
     ("gql"           . graphql-mode)
     ("sql"           . sql-mode)
     ("nix"           . nix-mode)
     ;; Build / infra
     ("dockerfile"    . dockerfile-mode)
     ("docker"        . dockerfile-mode)
     ("makefile"      . makefile-mode)
     ("make"          . makefile-mode)
     ("cmake"         . cmake-mode)
     ("nginx"         . nginx-mode)
     ("apache"        . apache-mode)
     ;; Misc
     ("markdown"      . markdown-mode)
     ("md"            . markdown-mode)
     ("diff"          . diff-mode)
     ("patch"         . diff-mode)
     ("tex"           . texinfo-mode)
     ("latex"         . texinfo-mode)))

  :config
  (+local-leader-set-key 'markdown-mode-map
    "l" '(markdown-toggle-url-hiding :wk "toggle URLs")
    "f" '(markdown-insert-footnote :wk "insert footnote")))

;;; Formatting

(use-package apheleia
  :defines apheleia-formatters
  :config
  (setf (alist-get 'deno-markdown apheleia-formatters)
        '("deno" "fmt" "--prose-wrap" "always" (apheleia-formatters-fill-column "--line-width") "--ext=md" "-"))

  (setf (alist-get 'prettier-markdown apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--parser=markdown" "--prose-wrap" "always" (apheleia-formatters-fill-column "--print-width")))

  (add-hook! (gfm-mode-local-vars)
    (setq-local apheleia-formatter
                (if (executable-find "deno")
                    'deno-markdown
                  'prettier-markdown))))

;;; init.el ends here
