terraform-mode

(o "output \"" (p "NAME") "\" {" n> "value = " q n "}" >)
(v "var \"" (p "NAME") "\" {" n> q n "}" >)
(r "resource \"" (p "TYPE") "\" \"" (p "NAME") "\" {" n> q n "}" >)
