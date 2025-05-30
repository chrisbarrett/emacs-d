(nil
 "\
locals {
  account_vars = read_terragrunt_config(find_in_parent_folders(\"account.hcl\"))
  region_vars  = read_terragrunt_config(find_in_parent_folders(\"region.hcl\"))

  account_name = local.account_vars.locals.account_name
  account_id   = local.account_vars.locals.aws_account_id
  aws_region   = local.region_vars.locals.aws_region

  bucket_prefix = \"TODO\"
}

remote_state {
  backend = \"s3\"

  generate = {
    path      = \"backend.tf\"
    if_exists = \"overwrite_terragrunt\"
  }

  config = {
    bucket         = \"${local.bucket_prefix}-tf-state-${local.account_name}-${local.aws_region}\"
    key            = \"${path_relative_to_include()}/tofu.tfstate\"
    region         = local.aws_region
    encrypt        = true
    dynamodb_table = \"tf-locks\"
  }
}

generate \"provider\" {
  path      = \"provider.tf\"
  if_exists = \"overwrite_terragrunt\"
  contents  = <<EOF
provider \"aws\" {
  region              = \"${local.aws_region}\"
  allowed_account_ids = [\"${local.account_id}\"]
}
EOF
}

inputs = merge(
  local.account_vars.locals,
  local.region_vars.locals,
)
")
