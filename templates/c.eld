c-mode c-ts-base-mode

(s "signed " q)
(u "unsigned " q)

(in "#include " q)
(d "#define " q)

(main "int main(void) {" n>
        q n>
        "return EXIT_SUCCESS;" n
      "}" >)

(i "if (" p ") {" n> q n "}" >)
(ei "else if (" p ") {" n q "}" >)
(e "else {" n> q n "}" >)
(ie "if (" p ") {" n> p n "} else {" > n> q n "}" >)
(r "return " q ";")
(wh "while (" p ") {" n> q n "}" >)
(dw "do {" n> p n "}" > " while (" q ");")

(for "for (" (p "INIT") "; " (p "PRED") "; " (p "ACTION") ") {" n>
     q n
     "}" >)

(sw "switch (" p ") {" n>
    "case " p ": {" > n>
    q n "}" >
    n "}" >
    )

(ca "case " p ": {" n>
    q n
    "}" >)

(st "struct " (p "NAME") " {" n>
    q n
    "};" >)

(tst "typedef struct " (p "NAME" name) " {" n>
    q n
    "} " > (s name) ";")

(un "union " (p "NAME") " {" n>
    q n
    "};" >)

(tun "typedef union " (p "NAME" name) " {" n>
    q n
    "} " > (s name) ";")

(p "printf(\"" (p "FORMAT") "\"" q ");")
