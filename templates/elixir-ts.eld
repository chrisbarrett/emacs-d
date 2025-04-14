elixir-ts-mode

(im "import " q)

(d "def " (p "NAME") " do" n> q n "end" >)
(dd "defp " (p "NAME") " do" n> q n "end" >)
(mo "defmodule " (p "NAME") " do" n> q n "end" >)
(ds "defstruct " q)

(dp "defprotocol " (p "NAME") " do" n> q > n "end" >)
(di "defimpl " (p "NAME") ", for: " (p "TY") " do" n> q > n "end" >)

(u "use " q)
(ca "case " (p "VAL") " do" n>
    (p "PAT") " -> " q n
    "end" >)

(re "receive do" n>
    (p "PAT") " -> " q n
    "end" >)
