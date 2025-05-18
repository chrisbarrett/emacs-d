rust-ts-mode

(ec "extern crate " q ";")
(u "use " q ";")
(l "let " p " = " q ";")
(lm "let mut " p " = " q ";")
(ep "eprintln!(\"" q "\");")
(c "const " p " = " q ";")

(i "if " (p "PRED") " {" n> q n "}" >)
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

(drv "#[derive(" q ")]")
