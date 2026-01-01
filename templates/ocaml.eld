neocaml-mode neocamli-mode

(t "type " p " = " q)

neocamli-mode

(v "val " p " : " q)

neocaml-mode

(l (+ocaml-capture-let-context) "let " p " = " q (+ocaml-maybe-in))
(lr (+ocaml-capture-let-context) "let rec " p " = " q (+ocaml-maybe-in))
(m "match " p " with " n "| " > q)
