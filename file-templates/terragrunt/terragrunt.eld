(nil
 "\
terraform {
  source = \"git::https://github.com/OWNER/REPO.git//.//PATH?ref=VERSION\"
}

include \"root\" {
  path   = find_in_parent_folders(\"root.hcl\")
}

inputs = {
}

")
