neocaml-mode

(l (+ocaml-capture-let-context) "let " p " = " q (+ocaml-maybe-in))
(lr (+ocaml-capture-let-context) "let rec " p " = " q (+ocaml-maybe-in))
(m "match " p " with " n "| " > q)
(t "type " p " = " q)
