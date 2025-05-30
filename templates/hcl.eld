hcl-mode

(o "output \"" (p "NAME") "\" {" n> "value = " q n "}" >)
(v "var \"" (p "NAME") "\" {" n> q n "}" >)
(r "resource \"" (p "TYPE") "\" \"" (p "NAME") "\" {" n> q n "}" >)

(g "generate \"" (p "NAME") "\" {" n>
   "path = \"" (p "FILE") "\"" n>
   "if_exists = \"overwrite_terragrunt\"" n>
   "contents = <<EOF" n
   q n
   "EOF" n
   "}" >)
