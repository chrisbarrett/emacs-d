;;; box-glyphs-tests.el --- Tests for box-glyphs -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for the `box-glyphs' leak-proof border-face primitive.

;;; Code:

(require 'ert)
(require 'box-glyphs)

(ert-deftest box-glyphs-normalised-face/inherits-base ()
  "The spec SHALL inherit the BASE face."
  (should (equal (plist-get (box-glyphs-normalised-face 'shadow) :inherit)
                 'shadow)))

(ert-deftest box-glyphs-normalised-face/clears-prose-styling ()
  "The spec SHALL clear prose text-styling attrs and pin the background."
  (let ((spec (box-glyphs-normalised-face 'shadow)))
    (should (eq (plist-get spec :slant) 'normal))
    (should (null (plist-get spec :underline)))
    (should (null (plist-get spec :overline)))
    (should (null (plist-get spec :strike-through)))
    (should (null (plist-get spec :box)))
    (should (equal (plist-get spec :background) "unspecified-bg"))))

(ert-deftest box-glyphs-normalised-face/applies-overrides ()
  "OVERRIDES SHALL be merged onto the base spec."
  (let ((spec (box-glyphs-normalised-face 'shadow :weight 'light)))
    (should (eq (plist-get spec :weight) 'light))
    ;; Base attrs survive alongside the override.
    (should (eq (plist-get spec :slant) 'normal))
    (should (equal (plist-get spec :background) "unspecified-bg"))))

(ert-deftest box-glyphs-normalised-face/accepts-composite-base ()
  "BASE MAY be a composite inherit list (e.g. (bold FACE))."
  (should (equal (plist-get (box-glyphs-normalised-face '(bold shadow)) :inherit)
                 '(bold shadow))))

(provide 'box-glyphs-tests)

;;; box-glyphs-tests.el ends here
