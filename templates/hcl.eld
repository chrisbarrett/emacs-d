hcl-mode

(d "dependency \"" (p "NAME" name) "\" {" n>
   "config_path  = \"../" name "\"" n>
   "mock_outputs = {" q "}" n
   "}" >)

(g "generate \"" (p "NAME") "\" {" n>
   "path = \"" (p "FILE") "\"" n>
   "if_exists = \"overwrite_terragrunt\"" n>
   "contents  = <<EOF" n
   q n
   "EOF" n
   "}" >)

(u "unit \"" (p "NAME" name) "\" {" n>
   "source = \"\"" n>
   "path   = \"" name "\"" n
   "}" >)

(st "stack \"" (p "NAME" name) "\" {" n>
   "source = \"\"" n>
   "path   = \"" name "\"" n>
   "values = {}" n
   "}" >)

(in "inputs = {" n>
    q n
    "}" >)

(inc "\
include \"" (p "root" id) "\" {
  path   = find_in_parent_folders(\"" id ".hcl\")
  expose = true
}")


hcl-mode terraform-mode

(l "locals {" n>
   q n
   "}" >)
