## Phase 1: Core infrastructure

- [x] 1.1 Add `+nix-bash-attrs` known-attrs defvar
- [x] 1.2 Add `+nix-lang-mode-alist` annotation→mode defvar
- [x] 1.3 Implement `+nix-ts--scan-spans` tree-sitter query function that returns sorted span list
- [x] 1.4 Add `+nix-ts--cached-spans` buffer-local var and cache rebuild on `treesit-parser-notifiers`

## Phase 2: Polymode matchers

- [x] 2.1 Implement `+nix-ts--head-matcher` function (forward/backward search on cache)
- [x] 2.2 Implement `+nix-ts--tail-matcher` function
- [x] 2.3 Implement `+nix-ts--mode-matcher` function (return mode name string from cache)
- [x] 2.4 Define `poly-nix-ts-auto-innermode` using `pm-inner-auto-chunkmode`
- [x] 2.5 Replace `poly-nix-ts-mode` innermodes list with single auto-innermode

## Phase 3: Code-fences integration

- [x] 3.1 Update `+nix--multiline-head-valid-p` to use tree-sitter instead of regex
- [x] 3.2 Add `:count-openers` callback using `+nix-ts--cached-spans` length
- [x] 3.3 Verify `:unquoted-p` and `:interpolation-fn` still work with new head boundaries

## Phase 4: Cleanup

- [x] 4.1 Remove 8 `define-innermode` forms and `+nix-poly--head-matcher` helper
- [x] 4.2 Keep `poly-nix-ts-hostmode` (still needed by `define-polymode`)
- [x] 4.3 Update tests for new detection patterns (annotation + attr-name)
