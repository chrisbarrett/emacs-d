rust-ts-mode

(ec "extern crate " q ";")
(u "use " q ";")
(l "let " p " = " q ";")
(lm "let mut " p " = " q ";")
(ep "eprintln!(\"" q "\");")
(c "const " p " = " q ";")
(t "type " p " = " q ";")

(i "if " (p "PRED") " {" n> q n "}" >)
(il "if let " p " = " p " {" n> q n "}" >)
(ei "else if " p " {" n q "}" >)
(e "else {" n> q n "}" >)
(ie "if " (p "PRED") " {" n> p n "} else {" > n> q n "}" >)
(r "return " q ";")

(m "match " (p "TERM") " {" n> q n "}" >)

(st "struct " (p "NAME") " {" n>
    q n
    "}" >)
(en "enum " (p "NAME") " {" n>
    q n
    "}" >)

(im "impl " (p "NAME") " {" n>
    q n
    "}" >)


(f "fn " (p "NAME") "(" p ")" p " {" n>
    q n
    "}" >)
(pf "pub fn " (p "NAME") "(" p ")" p " {" n>
    q n
    "}" >)

(drv "#[derive(" q ")]")
