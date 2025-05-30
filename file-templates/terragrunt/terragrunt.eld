(nil
 "\
include \"root\" {
  path   = find_in_parent_folders(\"root.hcl\")
  expose = true
}

locals {
  account_id      = include.root.locals.account_id
  region          = include.root.locals.aws_region
  source_base_url = include.root.locals.source_base_url
}

terraform {
  source = \"${local.source_base_url}//PATH?ref=VERSION\"
}

inputs = {
}

")
