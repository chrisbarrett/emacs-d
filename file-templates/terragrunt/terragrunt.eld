(nil
 "\
include \"root\" {
  path   = find_in_parent_folders(\"root.hcl\")
  expose = true
}

terraform {
  source = \"PATH?ref=VERSION\"
}

inputs = {
}

")
