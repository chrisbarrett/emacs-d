;;; mod-compilation.el --- Compilation output parsers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'compile)
(require '+compile)
(require 'general)

(setq compilation-always-kill t)
(setq compilation-ask-about-save nil) ; automatically save before compiling.
(setq compilation-scroll-output 'first-error)
(setq compilation-message-face 'default)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Change to look like a highlighted-line, rather than a visual selection.
(custom-theme-set-faces 'user
                        '(next-error-message ((t (:inherit hl-line)))))

;; Automatically truncate long compilation buffers.
(autoload 'comint-truncate-buffer "comint" nil t)
(remove-hook 'compilation-filter-hook #'comint-truncate-buffer)

;; Highlight URLs in compilation output & make them navigable.

(add-hook 'compilation-mode-hook
          (defun +compilation-ensure-keybindings ()
            (general-def :keymaps 'compilation-mode-map :states 'normal
              "RET"
              (general-predicate-dispatch #'compile-goto-error
                (thing-at-point 'url) #'goto-address-at-point))))

(autoload 'goto-address-fontify "goto-addr")

(add-hook 'compilation-filter-hook
          (defun +compilation-fontify-urls ()
            (goto-address-fontify compilation-filter-start (point))))


;;; Parsers

;; The `compile' command uses a pretty byzantine system to interpret outputs from
;; commands into error locations that can be navigated around in the editor.
;;
;; In particular:
;;
;; - The variable `compilation-error-regexp-alist-alist' defines a set of named
;;   error parsers, and
;;
;; - The variable `compilation-error-regexp-alist' determines which of those to
;;   actually use.
;;
;; Writing your own error parsers using this system is pretty finnicky. To make
;; working with it easier, I have defined a helper macro,
;; `define-compilation-error-rx'.


;; Start by disabling all built-in parsers; I only want to use the error parsers
;; I define in this file.

(setq compilation-error-regexp-alist nil)

;;; Generic compilation errors

;; Many well-behaved tools output something that vaguely conforms to the pattern
;; below.

(define-compilation-error-rx generic
  bol (* space) file ":" line-maybe-range (? ":" col-maybe-range) ": " message eol

  :where col-maybe-range = col (? "-" (+ digit))
  :where line-maybe-range = line (? "-" (+ digit))

  :hyperlink message)

(define-compilation-error-rx generic-no-message
  bol (* space) file ":" line-maybe-range (? ":" col-maybe-range) eol

  :where col-maybe-range = col (? "-" (+ digit))
  :where line-maybe-range = line (? "-" (+ digit)))


;;; JS, TypeScript, and associated tools

(define-compilation-error-rx node-warnings
  bol "(" (node-lit: "node") ":" (node-line: (+ digit)) ") " (warn-ident: (+? alnum) "Warning") ": " message eol
  :hyperlink message
  :highlights ((message 'compilation-warning)
               (warn-ident 'warning)
               (node-lit 'compilation-info)
               (node-line 'compilation-line-number))
  :type warning)

(define-compilation-error-rx js-error-stacktrace
  bol (+ space) (or type1 type2) eol

  :where type1 = (location-prefix: "at ") (src-info: (+? nonl)) " (" file ":" line ":" col ")"
  :where type2 = "\"" file-pat ":" line "\"" (? ",")

  :where file-pat = (? (location-prefix: "file://")) file
  :hyperlink file
  :highlights ((location-prefix 'shadow)
               (src-info 'compilation-info)
               (file 'compilation-info)
               (line 'compilation-line-number))
  :type info)

(define-compilation-error-rx typescript-tsc
  bol (* space) file ":" line ":" col " - " level " " err-code ": " message eol
  :where err-code = err-code: "TS" (+ digit)
  :where level = error: "error"

  :hyperlink message
  :highlights ((err-code 'font-lock-constant-face)
               (error 'compilation-error)))

;; Vitest

(define-compilation-error-rx vitest-trace-line
  bol (+ space) "❯ " message " " file ":" line ":" col
  :hyperlink message
  :type info)

(define-compilation-error-rx vitest-error
  bol (prefix: "Serialized Error") ": " message "\n"
  bol "This error originated in \"" file "\""
  :hyperlink message
  :highlights ((prefix 'compilation-error))
  :type error)


;;; Zig

(define-compilation-error-rx zig
  bol file ":" line ":" col ": " level ": " message eol
  :where level = (or (warn: "warn") (note: "note") (error: "error"))
  :type (warn . note)
  :hyperlink message
  :highlights ((warn 'warning)
               (error 'error)
               (note 'compilation-info)))

(define-compilation-error-rx zig-stack-line
  bol (= 4 space) fun ": " file ":" line ":" col
  :where fun = fun: (any alpha "_") (* (any alnum "_"))
  :hyperlink fun
  :type info
  :highlights ((fun 'font-lock-function-name-face)))


;;; Actionlint

(define-compilation-error-rx actionlint
  ;; .github/workflows/checks-deno.yml:0:0: could not parse as YAML: yaml: unknown anchor '.nix' referenced [syntax-check]
  bol file ":" line ":" col ":" (+ space) message code eol

  :where code = code: "[" (+? nonl) "]"

  :hyperlink message
  :highlights ((code 'font-lock-constant-face)))



;;; Terraform

(define-compilation-error-rx terraform
  ;; │ Error: Reference to undeclared input variable
  ;; │
  ;; │   on file.tf line 7, in resource "local_file" "terraform_tf":
  prefix (err: "Error: " message) "\n"
  prefix "\n"
  prefix (= 2 space) "on " (loc: file " line " line) (* nonl) "\n"
  :where prefix = bol "│ "
  :highlights ((err 'error))
  :hyperlink err)

;;; Terragrunt

(define-compilation-error-rx terragrunt
  bol "*" space (or validation-err module-process-err regular-err) eol

  :where validation-err =
  (err: "Validation failed") " for unit " unit " at path " file ": " message

  :where module-process-err =
  (err: "Cannot process module") space "Module " file space message

  :where regular-err =
  file ":" line "," col-ignoring-range ":" space message

  :where unit = (unit: (+ graphic))

  :where col-ignoring-range = col (? "-" (+ (any digit "-,")))

  :highlights ((unit 'compilation-info)
               (err 'compilation-error))
  :hyperlink message)

(define-compilation-error-rx terragrunt-err
  prefix (err: "Error: " message) "\n"
  prefix (= 2 space) "on " (loc: file " line " line) (* nonl) "\n"

  :where prefix = bol timestamp space "ERROR" (= 2 space) (? "[" unit "]" space)
  :where timestamp = (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "." (= 3 digit)
  :where unit = (unit: (+ graphic))

  :highlights ((err 'error)
               (unit 'compilation-info))
  :hyperlink err)

;; Errors in terragrunt stacks are reported from .terragrunt-stack; navigate to
;; actual input file instead.

(alist-set! compilation-transform-file-match-alist (rx "/.terragrunt-stack/") '("/"))
(alist-set! compilation-transform-file-match-alist (rx "/.terragrunt-stack" eos) '("/terragrunt.stack.hcl"))

;; Extra informational parsers.

(define-compilation-error-rx terragrunt-info
  (or "from" "at") space (? "'") (file: "./" (*? (not (any "\n:"))) ".hcl")
  (or "'"
      eol
      (and symbol-end " line " line (* nonl)))
  :file file
  :type info
  :hyperlink file)

(define-compilation-error-rx terragrunt-unit-operation
  bol timestamp space  (or non-error error-with-loc error-for-module
                           ;; TODO: Enable this if I want to stop on first encountered error
                           ;; any-error
                           )

  :where non-error =
  (or warn-lvl info-lvl) (+ space) "[" file "]" message eol

  :where error-with-loc =
  "ERROR" (+ space) "[" unit "] Error: " message "\n"
  timestamp space "ERROR" (+ space) "[" unit "]   on " (loc: file " line " line) (* nonl) eol

  :where error-for-module =
  "ERROR" (+ space) "[" unit "] Module " file space "has finished with an error" eol

  :where any-error =
  "ERROR" (+ space) "[" file "] " message eol

  :where warn-lvl = warn: "WARN"
  :where info-lvl = info: (or "INFO" "STDOUT")

  :where unit = unit: (+ graphic)

  :where timestamp = (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "." (= 3 digit)

  :type (warn . info)
  :highlights ((unit 'compilation-info))
  :hyperlink file)

(define-compilation-error-rx terragrunt-stack-modules
  bol "- Module " file (or eol space)
  :hyperlink file
  :type info)

(define-compilation-error-rx terragrunt-unit-reference
  prefix "Processing unit " (unit: (+ (not space))) " from " file eol

  :where prefix = bol timestamp space "INFO" (+ space)
  :where timestamp = (= 2 digit) ":" (= 2 digit) ":" (= 2 digit) "." (= 3 digit)

  :highlights ((unit 'compilation-info))
  :hyperlink file
  :type info)


;;; Elixir & the BEAM

(define-compilation-error-rx elixirc
  bol "** (" err-name ") " (or typespec-error err-at-loc mod-compile-err) eol
  :where typespec-error = file ":" line ": " message
  :where err-at-loc = message " on " file ":" line ":" col ":" (* nonl)
  :where mod-compile-err = file ": " message

  :where err-name = err-name: upper (* (any alnum "._"))
  :hyperlink message
  :highlights ((err-name 'error))
  :type error)

(define-compilation-error-rx elixir-mix
  bol (+ space) level ":" (* space) message (? " Did you mean:") "\n"
  (* "\n")
  (? (+ space) (or hint error-detail) "\n")
  (* (+ space) source-context "\n")
  bol (* space) "└─ " file ":" line ":" (? col (? ":" (* nonl)))

  :where level = (or (warn: "warning") (info: "info") "error")

  :where hint = "hint: " (* space) (hint-message: (+ nonl))

  :where error-detail = error-detail: alpha (+ nonl)

  :where source-context =  (or (and (? line-number) "│" (* nonl))
                               (and "*" (+ space) ident)
                               (and (* space) "..." (* nonl)))
  :where ident = (* nonl)
  :where line-number = (+ digit) (+ space)

  :type (warn . info)
  :highlights ((hint-message 'compilation-info)
               (error-detail 'compilation-info))
  :hyperlink message)

(define-compilation-error-rx elixir-test-failure
  bol (+ space) failure-number ") " message " (" module ")\n"
  bol (+ space) file ":" line
  :where module = module: upper (+ (any alnum "_."))
  :where failure-number = failure-number: (any "1-9") (* digit)
  :hyperlink message
  :highlights ((failure-number 'bold)
               (module 'font-lock-type-face)))

(define-compilation-error-rx elixir-test-stacktrace-line
  bol (+ space) (location: file ":" line) ": " source eol
  :where source = (or "(test)" (+ print))
  :hyperlink location
  :type info
  :highlights ((file 'compilation-info)))

(define-compilation-error-rx beam-stacktrace
  ;; (elixir 1.18.3) lib/gen_server.ex:1121: GenServer.call/3

  bol (+ space) "(" module " " version ") " (location: file ":" line) ": " message eol
  :where module = module: (+? any)
  :where version = version: (+? (any digit "."))
  :hyperlink location
  :type info
  :highlights ((file 'compilation-info)
               (module 'bold)
               (version 'font-lock-comment-face)))


;;; Rust

(define-compilation-error-rx rustc
  bol (* space) level (? code) ": " message "\n"
  bol (+ space) (arrow: "-->") " " file ":" line ":" col eol

  :where level = (or (err: "error")
                     (warn: "warning")
                     (info: (or "note" "help")))
  :where code = code: "[" (+ alnum) "]"

  :type (warn . info)
  :hyperlink message
  :highlights ((code 'font-lock-constant-face)
               (arrow 'shadow)
               (warn 'compilation-warning)
               (info 'compilation-info)
               (err 'compilation-error)))

(define-compilation-error-rx rust-panic
  bol "thread '" thread-name  "' panicked at "  file ":" line ":" col ":\n"
  (? err-type) message eol
  :where thread-name = thread-name: (+? graphic)
  :where err-type = err-type: "assertion failed: "
  :hyperlink file
  :highlights ((thread-name 'compilation-info)
               (message 'font-lock-string-face)
               (err-type 'error)))

(define-compilation-error-rx rust-stacktrace
  bol (or full-stack-frame reg-stack-frame) "\n"
  src-location

  :where full-stack-frame = (>= 1 space) depth ":" (+ space) mem-addr " - " fname
  :where reg-stack-frame = (>= 1 space) depth ":" (+ space) fname
  :where src-location = (+ space) "at " file ":" line ":" col

  :where depth = depth: (+ digit)
  :where mem-addr = mem-addr: (seq "0x" (+ digit))
  :where fname = fname: (+ nonl)

  :type info
  :highlights ((fname 'bold)
               (depth 'font-lock-constant-face)
               (mem-addr 'compilation-info)))


;;; tflint

(define-compilation-error-rx tflint
  ;; Warning: Missing version constraint for provider "aws" in `required_providers` (terraform_required_providers)
  ;;
  ;;   on trail.tf line 1:

  bol level ": " message " (" checker ")" eol "\n"
  bol eol "\n"
  bol (+ space) "on " file " line " line ":"

  :where level = (or (warn: "Warning") (note: "???") "Error")
  :where checker = checker: (+? (any alnum "_"))
  :type (warn . note)
  :highlights ((checker 'italic))
  :hyperlink message)


;;; Trivy

;; Unfortunately, Trivy's output is super-verbose; the file+col ref is *after*
;; an extended description of the error. Since multi-line parsers are likely to
;; break across `read-process-output-max' boundaries, I can't reliably associate
;; files with error messages.

(define-compilation-error-rx trivy-file
  ;; live/bootstrap/cloudtrail/main.tf (terraform)
  bol file " (terraform)" eol)

(provide 'mod-compilation)

;;; mod-compilation.el ends here
