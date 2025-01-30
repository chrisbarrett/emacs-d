typescript-ts-base-mode

;; See also ./javascript-base.eld, which contains shared snippets for all
;; javascript derivatives.

;; NB: the pattern `n } >' is used in templates that insert a terminal curly
;; brace; this is needed when the brace body starts empty, since
;; `newline-and-indent' does not know we are closing the group.

(t "type " p " = " q ";")
(et "export type " p " = " q ";")

(ns "namespace " p " {" n> q n "}" >)
(ens "export namespace " p " {" n> q n "}" >)

(zo "export const " p  " = z.object({" n> q n "});" >)
(zi "export type " (p "Identifier" ty) " = z.infer<typeof " q ty ">;")
(zin "export type In = z.input<typeof " q ">;")

;; complex Zod models
(zm "export const " (p "Identifier" ty) " = z.object({" n> q n> "});" n>
    n>
    "export type " ty " = z.infer<typeof " ty ">;" n>
    "export namespace " ty "{" n>
    "  export type In = z.input<typeof " q ">;" n
    "}" >
    )
