typescript-ts-base-mode js-base-mode

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
(ie "if (" p ") {" n> p n "} else {" n> q n "}" >)

(ed "export default " q ";")

(cl "class " p " {" n> q n "}" >)
(ecl "export class " p " {" n> q n "}" >)
