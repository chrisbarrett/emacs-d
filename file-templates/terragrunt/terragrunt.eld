(nil
 "\
include \"root\" {
  path = find_in_parent_folders(\"root.hcl\")
}

terraform {
  source = \"git::git@github.com:OWNER:REPO.git//PATH?ref=VERSION\"
}

inputs = {
}

")
