## 1. Attrpath reconstruction

- [x] 1.1 Add `+nix-ts--binding-attrpath` function that walks up tree-sitter parent nodes to build a full dotted attrpath string
- [x] 1.2 Test attrpath walk: flat style, nested style, let-binding, deeply nested with quoted attrs

## 2. Comment resolution with fboundp fallback

- [x] 2.1 Extract comment resolution into `+nix-ts--resolve-comment-mode` that tries alist then fboundp probe
- [x] 2.2 Test: alist match wins over fboundp, fboundp fallback works, neither matches returns nil

## 3. Attrpath regexp alist

- [x] 3.1 Replace `+nix-bash-attrs` with `+nix-attrpath-mode-alist` defvar containing regexp entries for all current bash attrs
- [x] 3.2 Add `+nix-ts--resolve-attrpath-mode` that runs attrpath against the regexp alist
- [x] 3.3 Test: leaf attr matches, full-path pattern matches, no match returns nil

## 4. Integrate into scan-spans

- [x] 4.1 Update `+nix-ts--scan-spans` to call attrpath walk, comment resolution, and attrpath resolution in correct order
- [x] 4.2 Extend span tuple to 6-tuple with attrpath string field
- [x] 4.3 Update all span tuple consumers (`+nix-ts--head-matcher`, `+nix-ts--tail-matcher`, `+nix-ts--mode-matcher`, `+nix--multiline-head-valid-p`, `+nix-ts--count-openers`) for 6-tuple

## 5. Integration tests

- [x] 5.1 Test full scan with comment fboundp mode (mock may-i-config-mode via fset)
- [x] 5.2 Test full scan with attrpath regexp match overriding default (no annotation)
- [x] 5.3 Test comment annotation still overrides attrpath match
