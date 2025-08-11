(nil
 "\
terraform {
  source = \"PATH?ref=VERSION\"
}

include \"root\" {
  path   = find_in_parent_folders(\"root.hcl\")
  expose = true
}

inputs = {
}

")
