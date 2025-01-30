typescript-mode typescript-ts-mode

;; NB: the pattern `n } >' is used in templates that insert a terminal curly
;; brace; this is needed when the brace body starts empty, since
;; `newline-and-indent' does not know we are closing the group.

(l "let " p " = " q ";")
(c "const " p " = " q ";")
(ec "export const " p " = " q ";")

(f "const " p " = (" p ") => " q ";")
(af "const " p " = async (" p ") => " q ";")
(eaf "export const " p " = async (" p ") => " q ";")
(fun "function " p "(" p ") {" n> q n "}" >)
(afun "async function " p "(" p ") {" n> q n "}" >)
(eafun "export async function " p "(" p ") {" n> q n "}" >)

(i "if (" p ") {" n> q n "}" >)
(ei "else if (" p ") {" n q "}" >)
(e "else {" n> q n "}" >)

(ed "export default " q ";")

(t "type " p " = " q ";")
(et "export type " p " = " q ";")

(ns "namespace " p " {" n> q n "}" >)
(ens "export namespace " p " {" n> q n "}" >)

(zo "export const " (p ty) " = z.object({" n> q n "});" >)
(zi "export type " (p ty) " = z.infer<typeof " (q ty) ">;")
(zin "export type In = z.input<typeof " q ">;")

;; complex Zod models
(zm "export const " (p ty) " = z.object({" n> q n> "});" n>
    n>
    "export type " ty " = z.infer<typeof " ty ">;" n>
    "export namespace " ty "{" n>
    "  export type In = z.input<typeof " q ">;" n
    "}" >
    )
