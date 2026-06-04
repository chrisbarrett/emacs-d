;;; box-glyphs.el --- Leak-proof border-face primitive for overlay box decorators -*- lexical-binding: t; -*-

;;; Commentary:

;; A dependency-free home for the one border-face incantation that every
;; overlay box decorator needs and is easy to get subtly wrong.  Both
;; `argc-mode' and the `gfm-pretty' border decorators draw Unicode box
;; rules with overlay strings that share buffer regions with prose
;; font-lock and text-property backgrounds; without neutralising the
;; right attributes those leak into the border glyphs.  Keeping the
;; knowledge in one tested place stops the two copies from drifting.

;;; Code:

(defun box-glyphs-normalised-face (base &rest overrides)
  "Return a face spec inheriting BASE with prose styling neutralised.

Border glyphs share buffer regions with prose whose font-lock face
may carry `:slant italic', `:underline t', etc.  Without an explicit
override those attrs leak through face composition on GUI frames and
visually slant or decorate the box edges, so the spec pins
`:slant normal' and clears `:underline', `:overline',
`:strike-through', and `:box'.

`:background' is pinned to the literal Emacs marker
`\"unspecified-bg\"' (the system / frame background) so a buffer
position's text-property `:background' — e.g. `diff-added' on the
newline at a body line's end — does not bleed through into border,
before-string, or after-string chars whose `:background' would
otherwise be unspecified and inherit from below.

BASE is anything accepted by `:inherit' (a face symbol or a list such
as (bold FACE)).  OVERRIDES is a plist merged on top of the base
spec, e.g. a `:weight' of `light'."
  (let ((spec `(:inherit ,base
                :slant normal
                :underline nil :overline nil :strike-through nil :box nil
                :background "unspecified-bg")))
    (while overrides
      (setq spec (plist-put spec (pop overrides) (pop overrides))))
    spec))

(provide 'box-glyphs)

;;; box-glyphs.el ends here
